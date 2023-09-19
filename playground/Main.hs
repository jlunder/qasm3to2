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

data Ref = NilRef | Ref Int deriving (Eq, Ord, Read, Show)

type SemanticNode = AstNode Tag SemanticInfo

data SemanticInfo = SemanticInfo
  { infoExpressionType :: ExpressionType,
    infoIdentifierAttributes :: IdentifierAttributes
  }
  deriving (Eq, Read, Show)

initialSemanticInfo :: SemanticInfo
initialSemanticInfo = SemanticInfo NilType initialIdentifierAttributes

data IdentifierAttributes = IdentifierAttributes
  { identName :: String,
    identRef :: Ref,
    identScopeRef :: Ref,
    identType :: ExpressionType,
    identValue :: ConstantValue
  }
  deriving (Eq, Read, Show)

initialIdentifierAttributes :: IdentifierAttributes
initialIdentifierAttributes =
  IdentifierAttributes
    { identName = "",
      identRef = NilRef,
      identScopeRef = NilRef,
      identType = NilType,
      identValue = NilValue
    }

withSemanticInfo :: AstNode Tag c -> SemanticNode
withSemanticInfo NilNode = NilNode
withSemanticInfo (AstNode tag children _) =
  AstNode tag (map withSemanticInfo children) initialSemanticInfo

type SemanticStateM a = State.State SemanticState a

data SemanticState = SemanticState
  { stateNextRefId :: Int,
    stateErrors :: [String]
  }
  deriving (Eq, Read, Show)

initialSemanticState :: SemanticState
initialSemanticState = SemanticState {stateNextRefId = 2, stateErrors = []}

data SemanticGraph = SemanticGraph
  { -- Types are fused into identifiers -- could have separated into a type constraints map/list
    semGraphIdentifiers :: Map.Map Ref IdentifierAttributes,
    semGraphTypes :: Map.Map Ref ExpressionType,
    semGraphExpressions :: Map.Map Ref SemanticNode,
    semGraphValues :: Map.Map Ref ConstantValue,
    semGraphScopes :: Map.Map Ref LexicalScope
  }
  deriving (Eq, Read, Show)

initialSemanticGraph :: SemanticGraph
initialSemanticGraph =
  SemanticGraph
    { semGraphIdentifiers = Map.empty,
      semGraphTypes = Map.empty,
      semGraphExpressions = Map.empty,
      semGraphValues = Map.empty,
      semGraphScopes = Map.empty
    }

semGraphRootScopeRef :: Ref
semGraphRootScopeRef = Ref 1

data LexicalScope = LexicalScope
  { scopeRef :: Ref,
    scopeParentRef :: Ref,
    scopeIdentifiers :: Map.Map String IdentifierAttributes
  }
  deriving (Eq, Read, Show)

initialScope :: LexicalScope
initialScope = LexicalScope NilRef NilRef Map.empty

data ConstantValue
  = NilValue
  | IntValue Int
  | TupleValue [ConstantValue]
  deriving (Eq, Read, Show)

uniqueRef :: SemanticStateM Ref
uniqueRef = do
  state <- State.get
  let refId = stateNextRefId state
  State.put (state {stateNextRefId = refId + 1})
  return $ Ref refId

expressionTypeFromNode :: p -> ExpressionType
expressionTypeFromNode node = NilType -- TODO

addSemanticError :: String -> SemanticStateM ()
addSemanticError errStr = do
  state <- State.get
  State.put (state {stateErrors = errStr : stateErrors state})

semanticTreeResolveIdentifiers :: LexicalScope -> SemanticNode -> SemanticStateM (LexicalScope, SemanticNode)
--
-- ScopeStmt: for each statement in the scope, update the lexical scope (for Let)
semanticTreeResolveIdentifiers scope node@(AstNode ScopeStmt children info) = do
  (scope, newRevChildren) <- accumulateStatementScopes scope [] children
  return (scope, node {astChildren = reverse newRevChildren})
  where
    accumulateStatementScopes accumScope accumRevChildren [] = return (accumScope, accumRevChildren)
    accumulateStatementScopes accumScope accumRevChildren (child : childrenRemain) = do
      (newAccumScope, newChild) <- semanticTreeResolveIdentifiers accumScope child
      accumulateStatementScopes newAccumScope (newChild : accumRevChildren) childrenRemain
--
-- LetStmt: add the identifier to the merged scope and update it, pass new scope into the init expression
semanticTreeResolveIdentifiers scope node@(AstNode LetStmt children _) =
  let [declTypeNode, declIdentNode, initExprNode] = children
      declType = expressionTypeFromNode declTypeNode
      AstNode (Identifier declName) _ _ = declIdentNode
   in do
        newIdentRef <- uniqueRef
        newScopeRef <- uniqueRef
        let identAttrs =
              initialIdentifierAttributes
                { identName = declName,
                  identRef = newIdentRef,
                  identScopeRef = scopeRef scope,
                  identType = declType
                }
            newIdentMap = Map.insert declName identAttrs (scopeIdentifiers scope)
            newScope = scope {scopeRef = newScopeRef, scopeParentRef = scopeRef scope, scopeIdentifiers = newIdentMap}
        newPairs <- mapM (semanticTreeResolveIdentifiers newScope) children
        return (newScope, node {astChildren = map snd newPairs})
--
-- Identifier: find the identifier in the scope and decorate the node with its attributes
semanticTreeResolveIdentifiers scope node@(AstNode (Identifier name) [] _) =
  case scopeIdentifiers scope Map.!? name of
    Nothing -> do addSemanticError ("Could not resolve identifier " ++ name); return (scope, node)
    Just identAttrs ->
      return (scope, node {astContext = (astContext node) {infoIdentifierAttributes = identAttrs}})
--
-- Anything else: pass current scope through to children
semanticTreeResolveIdentifiers scope node@(AstNode _ children _) = do
  newPairs <- mapM (semanticTreeResolveIdentifiers scope) children
  return (scope, node {astChildren = map snd newPairs})

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
  let ((globalScope, semRoot), semCtx) =
        runState
          ( do
              let scopedG = withSemanticInfo testAst
              globalScopeRef <- uniqueRef
              let globalScope = initialScope {scopeRef = globalScopeRef}
              semanticTreeResolveIdentifiers globalScope scopedG
          )
          initialSemanticState

  putStrLn "Semantic Graph:"
  print semRoot
  putStrLn ""
  putStrLn "Program Context:"
  print semCtx
  putStrLn ""
  return ()
