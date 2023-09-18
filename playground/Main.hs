module Main where

import Control.Applicative (Alternative (empty))
import Control.Monad
import Control.Monad.State (runState)
import Control.Monad.State qualified as State
import Data.Map.Strict qualified as Map
import Data.Maybe
import Debug.Trace

-- import Data.Traversable

data AstNode t c where
  NilNode :: AstNode t c
  AstNode :: {astTag :: t, astChildren :: [AstNode t c], astContext :: c} -> AstNode t c
  deriving (Eq, Read, Show)

isNilNode :: AstNode t c -> Bool
isNilNode NilNode = True
isNilNode _ = False

data Tag
  = Program -- [global-scope]
  | ScopeStmt -- [stmts..]
  | LetStmt -- [type, ident, init-expr]
  | AssignStmt -- [ident, expr]
  | IfElseStmt -- [expr, scope, scope]
  | WhileStmt -- [expr, scope]
  | BreakStmt -- []
  | ParameterList -- []
  | BinaryExpr String -- [expr, expr]
  | UnaryExpr String -- [expr]
  | FunctionExpr -- [paramlist, scope]
  | Identifier String -- []
  | IntLiteral Int -- []
  | TupleCtor -- [element-exprs..]
  | IntType -- []
  | TupleType -- [types..]
  | FunctionType -- [in-type, out-type]
  deriving (Eq, Read, Show)

data ExpressionType
  = NilType
  | ExprInt
  | ExprTuple [ExpressionType]
  | ExprFunction ExpressionType ExpressionType
  deriving (Eq, Read, Show)

newtype Ref = Ref Int deriving (Eq, Ord, Read, Show)

type SemanticNode = AstNode Tag SemanticInfo

data SemanticInfo = SemanticInfo
  { expressionType :: ExpressionType,
    scopeRef :: Maybe Ref,
    identifierRef :: Maybe Ref
  }
  deriving (Eq, Read, Show)

initialSemanticInfo :: SemanticInfo
initialSemanticInfo = SemanticInfo NilType Nothing Nothing

withSemanticInfo :: AstNode Tag c -> SemanticNode
withSemanticInfo NilNode = NilNode
withSemanticInfo (AstNode tag children _) =
  AstNode tag (map withSemanticInfo children) initialSemanticInfo

type SemanticStateM s a = State.State (SemanticProgramContext, s) a

data SemanticProgramContext = SemanticProgramContext
  { stateAllIdentifiers :: Map.Map Ref IdentifierAttributes,
    stateAllScopes :: Map.Map Ref LexicalScope,
    stateRootScopeRef :: Ref,
    stateTypeVariables :: Map.Map Int Int,
    stateNextRefId :: Int,
    stateErrors :: [String]
  }
  deriving (Eq, Read, Show)

initialProgramContext :: SemanticProgramContext
initialProgramContext =
  SemanticProgramContext
    { stateAllIdentifiers = Map.empty,
      stateAllScopes = Map.insert (Ref 1) (LexicalScope Map.empty Nothing) Map.empty,
      stateRootScopeRef = Ref 1,
      stateTypeVariables = Map.empty,
      stateNextRefId = 2,
      stateErrors = []
    }

data IdentifierAttributes = IdentifierAttributes
  { identifierName :: String,
    identifierValue :: Maybe ConstantValue
  }
  deriving (Eq, Read, Show)

data LexicalScope = LexicalScope
  { identifiers :: Map.Map String Ref,
    parentScope :: Maybe Ref
  }
  deriving (Eq, Read, Show)

data ConstantValue
  = IntValue Int
  | TupleValue [ConstantValue]
  deriving (Eq, Read, Show)

addScope :: Maybe Ref -> SemanticStateM s Ref
addScope parentScopeRef = do
  newRef <- uniqueRef
  (state, inner) <- State.get
  let newScope = Map.insert newRef (LexicalScope Map.empty parentScopeRef) (stateAllScopes state)
  State.put (state {stateAllScopes = newScope}, inner)
  return newRef

uniqueRef :: SemanticStateM s Ref
uniqueRef = do
  (state, inner) <- State.get
  let refId = stateNextRefId state
  State.put (state {stateNextRefId = refId + 1}, inner)
  return $ Ref refId

addError :: String -> SemanticStateM s ()
addError errorStr = do
  (state, inner) <- State.get
  State.put (state {stateErrors = errorStr : stateErrors state}, inner)
  return ()

threadSemanticState :: SemanticStateM a c -> (SemanticProgramContext -> a) -> SemanticStateM b c
threadSemanticState f inner = do
  (ctx, oldInner) <- State.get
  let (result, (newCtx, _)) = runState f (ctx, inner ctx)
  State.put (newCtx, oldInner)
  return result

semanticTreeAssignScopes :: SemanticNode -> SemanticStateM () SemanticNode
semanticTreeAssignScopes rootNode =
  threadSemanticState (recurseAssignScopes rootNode) (Just . stateRootScopeRef)
  where
    changeParentScope :: Maybe Ref -> SemanticStateM (Maybe Ref) (Maybe Ref)
    changeParentScope parentScopeRef = do
      (state, oldParentScopeRef) <- State.get
      State.put (state, parentScopeRef)
      return oldParentScopeRef

    withParentScope :: Maybe Ref -> SemanticStateM (Maybe Ref) a -> SemanticStateM (Maybe Ref) a
    withParentScope parentScopeRef m = do
      oldScopeRef <- changeParentScope parentScopeRef
      res <- m
      _ <- changeParentScope oldScopeRef
      return res

    doAssignNewScope :: SemanticInfo -> SemanticStateM (Maybe Ref) (SemanticInfo, Ref)
    doAssignNewScope semInfo = do
      (_, parentScopeRef) <- State.get
      newScope <- addScope parentScopeRef
      return (semInfo {scopeRef = Just newScope}, newScope)

    recurseAssignScopes :: SemanticNode -> SemanticStateM (Maybe Ref) SemanticNode
    recurseAssignScopes NilNode = return NilNode
    recurseAssignScopes (AstNode tag children semInfo) =
      case tag of
        ScopeStmt -> do
          (newInfo, newScope) <- doAssignNewScope semInfo
          newChildren <- withParentScope (Just newScope) (mapM recurseAssignScopes children)
          return (AstNode tag newChildren newInfo)
        LetStmt -> do
          (newInfo, newScope) <- doAssignNewScope semInfo
          _ <- changeParentScope $ Just newScope
          newChildren <- mapM recurseAssignScopes children
          return (AstNode tag newChildren newInfo)
        _ -> do
          newChildren <- mapM recurseAssignScopes children
          (_, parentScopeRef) <- State.get
          return (AstNode tag newChildren (semInfo {scopeRef = parentScopeRef}))

semanticTreeResolveIdentifiers :: SemanticNode -> SemanticStateM () SemanticNode
semanticTreeResolveIdentifiers = recurseResolveIdentifiers
  where
    resolveIdentifier :: String -> Maybe Ref -> SemanticStateM a (Maybe Ref)
    resolveIdentifier name Nothing = trace ("missing " ++ name) (return Nothing)
    resolveIdentifier name (Just scopeRef) = do
      (context, _) <- State.get
      let LexicalScope
            { identifiers = identMap,
              parentScope = parentScope
            } = stateAllScopes context Map.! scopeRef
      case identMap Map.!? name of
        Nothing -> resolveIdentifier name parentScope
        Just identRef ->
          trace
            ("found " ++ name ++ " as " ++ show identRef)
            (return $ Just identRef)

    addIdentifier :: String -> Ref -> SemanticStateM a Ref
    addIdentifier name scopeRef = do
      (context, _) <- State.get
      let allScopes = stateAllScopes context
      let scope = allScopes Map.! scopeRef
      let identMap = identifiers scope
      newIdentRef <- uniqueRef
      let newIdentMap = Map.insert name newIdentRef identMap
      let newAllScopes = Map.insert scopeRef (scope {identifiers = newIdentMap}) allScopes
      (newContext, someA) <- State.get
      State.put (newContext {stateAllScopes = newAllScopes}, someA)
      trace ("add " ++ name ++ " to " ++ show scopeRef) (return ())
      return newIdentRef

    recurseResolveIdentifiers :: SemanticNode -> SemanticStateM () SemanticNode
    recurseResolveIdentifiers NilNode = return NilNode
    recurseResolveIdentifiers node@(AstNode tag children semInfo) =
      case tag of
        LetStmt -> do
          let [declaringType, declaringIdentifier, initExpr] = children
          let AstNode (Identifier declaringName) _ _ = declaringIdentifier
          case scopeRef semInfo of
            Nothing -> undefined -- previous pass failed to assign a scope?
            Just declaringScopeRef -> do
              identRef <- addIdentifier declaringName declaringScopeRef
              newInitExpr <- recurseResolveIdentifiers initExpr
              resolveChildren semInfo
        Identifier name -> do
          case scopeRef semInfo of
            Nothing -> undefined -- previous pass failed to assign a scope?
            identScopeRef@(Just _) -> do
              maybeIdentRef <- resolveIdentifier name (scopeRef semInfo)
              when (isNothing maybeIdentRef) $ addError ("Could not resolve identifier " ++ name) -- at...
              resolveChildren $ semInfo {identifierRef = maybeIdentRef}
        _ -> resolveChildren semInfo
      where
        resolveChildren semInfo = do
          newChildren <- mapM recurseResolveIdentifiers children
          return (AstNode tag newChildren semInfo)

testAst =
  AstNode
    Program
    [ AstNode
        ScopeStmt
        [ AstNode LetStmt [AstNode IntType [] (), AstNode (Identifier "x") [] (), AstNode (IntLiteral 0) [] ()] (),
          AstNode
            LetStmt
            [ AstNode IntType [] (),
              AstNode (Identifier "y") [] (),
              AstNode (BinaryExpr "+") [AstNode (Identifier "x") [] (), AstNode (IntLiteral 5) [] ()] ()
            ]
            (),
          AstNode
            WhileStmt
            [ AstNode (BinaryExpr "<") [AstNode (Identifier "x") [] (), AstNode (IntLiteral 5) [] ()] (),
              AstNode
                ScopeStmt
                [ AstNode
                    AssignStmt
                    [ AstNode (Identifier "x") [] (),
                      AstNode (BinaryExpr "+") [AstNode (Identifier "x") [] (), AstNode (IntLiteral 1) [] ()] ()
                    ]
                    ()
                ]
                ()
            ]
            ()
        ]
        ()
    ]
    ()

main :: IO ()
main = do
  let (semGraph, (semCtx, _)) =
        runState
          ( do
              let initialG = withSemanticInfo testAst
              scopedG <- semanticTreeAssignScopes initialG
              semanticTreeResolveIdentifiers (trace (show scopedG) scopedG)
          )
          (initialProgramContext, ())

  putStrLn "Semantic Graph:"
  print semGraph
  putStrLn ""
  putStrLn "Program Context:"
  print semCtx
  putStrLn ""
  return ()
