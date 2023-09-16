module Main where

import Control.Applicative (Alternative (empty))
import Control.Monad
import Control.Monad.State (runState)
import Control.Monad.State qualified as State
import Data.Map.Lazy qualified as Map
import Data.Maybe (fromMaybe)
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

data SemanticProgramContext
  = SemanticProgramContextError
      {stateErrorMessage :: String}
  | SemanticProgramContextOkay
      { stateAllIdentifiers :: Map.Map Ref IdentifierAttributes,
        stateAllScopes :: Map.Map Ref LexicalScope,
        stateRootScopeRef :: Ref,
        stateTypeVariables :: Map.Map Int Int,
        stateNextRefId :: Int
      }
  deriving (Eq, Read, Show)

initialProgramContext :: SemanticProgramContext
initialProgramContext =
  SemanticProgramContextOkay
    { stateAllIdentifiers = Map.empty,
      stateAllScopes = Map.insert (Ref 1) (LexicalScope Map.empty Nothing) Map.empty,
      stateRootScopeRef = Ref 1,
      stateTypeVariables = Map.empty,
      stateNextRefId = 2
    }

data IdentifierAttributes = IdentifierAttributes
  { identifierName :: String,
    identifierValue :: Maybe ConstantValue
  }
  deriving (Eq, Read, Show)

data LexicalScope = LexicalScope
  { identifiers :: Map.Map String Int,
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

semanticTreeAssignScopes :: SemanticNode -> (SemanticNode, SemanticProgramContext)
semanticTreeAssignScopes rootNode =
  let changeParentScope :: Maybe Ref -> SemanticStateM (Maybe Ref) (Maybe Ref)
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

      (result, (ctx, _)) =
        runState
          (recurseAssignScopes rootNode)
          (initialProgramContext, Just $ stateRootScopeRef initialProgramContext)
   in (result, ctx)

{-

-- Add declared identifiers into the appropriate lexical scopes.
semanticTreeAssignDeclaringIdentifiers :: SemanticNode -> SemanticStateM SemanticNode
semanticTreeAssignDeclaringIdentifiers NilNode = return NilNode
semanticTreeAssignDeclaringIdentifiers rootNode =
  let doAssignDeclaringIdentifiers :: Ref -> SemanticInfo -> SemanticStateM SemanticInfo
      doAssignDeclaringIdentifiers scopeRef semInfo = do
        newScope <- addScope scopeRef
        return (semInfo {scopeRef = Just newScope})
      recurseAssignDeclaringIdentifiers :: SemanticNode -> SemanticStateM SemanticNode
      recurseAssignDeclaringIdentifiers node@(AstNode tag children semInfo) = do
        newSemInfo <- case tag of
          AliasDeclStmt ->
            let identNode = children !! 0
             in return (semInfo {identifierRef = Just (Ref 1)}) -- [Identifier, Expression..]
          ClassicalDeclStmt ->
            let identNode = children !! 1
             in return (semInfo {identifierRef = Just (Ref 1)}) -- [ScalarTypeSpec | ArrayTypeSpec, Identifier, DeclarationExpr?]
          ConstDeclStmt ->
            let identNode = children !! 1
             in return (semInfo {identifierRef = Just (Ref 1)}) -- [ScalarTypeSpec, Identifier, DeclarationExpr]
          DefStmt ->
            let identNode = children !! 0
             in return (semInfo {identifierRef = Just (Ref 1)}) -- [Identifier, List<ArgumentDefinition>, ScalarTypeSpec?, Scope]
          DefcalStmt ->
            let identNode = children !! 0
             in return (semInfo {identifierRef = Just (Ref 1)}) -- [DefcalTarget, List<(Expression | ArgumentDefinition)>?, List<HardwareQubit | Identifier>, ScalarTypeSpec?, CalBlock]
          ExternStmt -> return (semInfo {identifierRef = Just (Ref 1)}) -- [Identifier, List<ScalarTypeSpec>, returnTypeSpec::ScalarTypeSpec?]
          GateStmt -> return (semInfo {identifierRef = Just (Ref 1)}) -- [Identifier, List<Identifier>?, List<Identifier>, Scope]
          CregOldStyleDeclStmt -> return (semInfo {identifierRef = Just (Ref 1)}) -- [Identifier, designator::Expression?]
          QregOldStyleDeclStmt -> return (semInfo {identifierRef = Just (Ref 1)}) -- [Identifier, designator::Expression?]
          QuantumDeclStmt -> return (semInfo {identifierRef = Just (Ref 1)}) -- [QubitTypeSpec, Identifier]
          _ -> return semInfo
        newChildren <- mapM recurseAssignDeclaringIdentifiers children
        return (AstNode tag newChildren newSemInfo)
   in do
        state <- State.get
        recurseAssignDeclaringIdentifiers rootNode

data Tag
  = Program -- [global-scope]
  | ScopeStmt -- [stmts..]
  | LetStmt -- [type, ident, init-expr]
  | IfElseStmt -- [expr, scope, scope]
  | WhileStmt -- [expr, scope]
  | BreakStmt -- []
  | ParameterList -- []
  | BinaryExpr -- [expr, expr]
  | UnaryExpr String -- [expr]
  | FunctionExpr -- [paramlist, scope]
  | Identifier String -- []
  | IntLiteral Int -- []
  | TupleCtor -- [element-exprs..]
  | IntType -- []
  | TupleType -- [types..]
  | FunctionType -- [in-type, out-type]
  deriving (Eq, Read, Show)

-}

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
  let (semGraph, semPrgCtx) = semanticTreeAssignScopes $ withSemanticInfo testAst
  putStrLn "Semantic Graph:"
  print semGraph
  putStrLn ""
  putStrLn "Program Context:"
  print semPrgCtx
  putStrLn ""
  return ()
