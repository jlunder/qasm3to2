module Qasm3SemanticAnalyzer where

import Ast
import Control.Monad.State qualified as State
import Data.Int (Int64)
import Data.Map.Lazy qualified as Map
import Data.Word (Word64)
import Qasm3

newtype Ref = Ref Int deriving (Eq, Ord, Read, Show)

data TypeParameter a = LiteralParam a | VariableParam Ref
  deriving (Eq, Ord, Read, Show)

type SemanticNode = AstNode Tag SemanticAnnotation

newtype SemanticAnnotation = SemanticAnnotation Int
  deriving (Eq, Ord, Read, Show)

data IdentifierAttributes = IdentifierAttributes
  { identifierName :: String,
    identifierValue :: Maybe ConstantValue
  }
  deriving (Eq, Read, Show)

data LexicalScope = LexicalScope
  { identifiers :: Map.Map String Int,
    parentScope :: Int
  }
  deriving (Eq, Read, Show)

type SemanticStateM a = State.State SemanticState a

data SemanticState = SemanticState
  { allIdentifiers :: Map.Map Ref IdentifierAttributes,
    allScopes :: Map.Map Ref LexicalScope,
    typeVariables :: Map.Map Int Int,
    nextRefId :: Int
  }
  deriving (Eq, Read, Show)

initialState :: SemanticState
initialState =
  SemanticState
    { allIdentifiers = Map.empty,
      allScopes = Map.empty,
      typeVariables = Map.empty,
      nextRefId = 1
    }

semanticTreeFrom :: AstNode Tag c -> SemanticNode
semanticTreeFrom NilNode = NilNode
semanticTreeFrom rootNode =
  let (outTree, outState) =
        State.runState
          ( do
              scopesTree <- semanticTreeAssignScopes rootNode
              identsTree <- semanticTreeGatherIdentifiers scopesTree
              typesTree <- semanticTreeResolveTypes identsTree
              return NilNode
          )
          initialState
   in outTree

-- get scope hierarchy?
--
-- case tag of
--   Identifier identifier _ -> lookupOrAddIdentifier identifier
--   _ -> return ()
-- mapM_ semanticTreeFrom children
-- return NilNode

semanticTreeAssignScopes :: AstNode Tag c -> SemanticStateM SemanticNode
semanticTreeAssignScopes NilNode = return NilNode
semanticTreeAssignScopes (AstNode tag children _) = do
  newChildren <- mapM semanticTreeAssignScopes children
  return (AstNode tag newChildren (SemanticAnnotation 0))

semanticTreeGatherIdentifiers :: SemanticNode -> SemanticStateM SemanticNode
semanticTreeGatherIdentifiers NilNode = return NilNode
semanticTreeGatherIdentifiers (AstNode tag children _) = do
  newChildren <- mapM semanticTreeAssignScopes children
  return (AstNode tag newChildren (SemanticAnnotation 0))

semanticTreeResolveTypes :: SemanticNode -> SemanticStateM SemanticNode
semanticTreeResolveTypes NilNode = return NilNode
semanticTreeResolveTypes (AstNode tag children _) = do
  newChildren <- mapM semanticTreeAssignScopes children
  return (AstNode tag newChildren (SemanticAnnotation 0))

{-
lookupOrAddIdentifier :: String -> SemanticStateM ()
lookupOrAddIdentifier identifier = do
  ref <- uniqueRef
  state <- State.get
  -- find identifier in scope
  -- already there? match to existing one
  -- missing? make a new one
  let updatedIdentifiers = Map.insert ref (IdentifierAttributes identifier) (allIdentifiers state)
  State.put (state {allIdentifiers = updatedIdentifiers})
-}

uniqueRef :: SemanticStateM Ref
uniqueRef = do
  state <- State.get
  let refId = nextRefId state
  State.put state {nextRefId = refId + 1}
  return $ Ref refId

data ScalarType
  = ErrorType
  | BitType {scalarBitSize :: TypeParameter Int}
  | IntType {scalarBitSize :: TypeParameter Int}
  | UintType {scalarBitSize :: TypeParameter Int}
  | FloatType {scalarBitSize :: TypeParameter Int}
  | AngleType {scalarBitSize :: TypeParameter Int}
  | BoolType
  | DurationType {scalarStretch :: TypeParameter Bool}
  | ComplexType {scalarBitSize :: TypeParameter Int}
  deriving (Eq, Read, Show)

data ExpressionType
  = ScalarExpr {exprScalarType :: TypeParameter ScalarType}
  | QubitExpr {exprQubitBits :: TypeParameter Int}
  | HardwareQubitExpr {exprHwQubitIndex :: Int}
  | IndexExpr {}
  | ArrayExpr {mutable :: Bool, elementType :: ScalarType, dimensions :: [TypeParameter Int]}
  deriving (Eq, Read, Show)

-- Reducing duration expressions is nontrivial
data Duration
  = ZeroDuration
  | ExactDtDuration Double
  | ExactNsDuration Double
  | StretchDuration
  | SumDuration [Duration]
  | MaxDuration [Duration]
  deriving (Eq, Read, Show)

data ConstantValue
  = BitValue Int Int64
  | IntValue Int Int64
  | UintValue Int Word64
  | FloatValue Int Double
  | AngleValue Int Double
  | BoolValue Bool
  | DurationValue Bool Double
  | ComplexValue Int Double Double
  | ArrayValue ScalarType [ConstantValue]
  deriving (Eq, Read, Show)

-- coerceToCompatible
-- coerceToIntegral

typeOfValue :: ConstantValue -> ScalarType
typeOfValue (BitValue bits _) = BitType $ LiteralParam bits
typeOfValue (IntValue bits _) = IntType $ LiteralParam bits
typeOfValue (UintValue bits _) = UintType $ LiteralParam bits
typeOfValue (FloatValue bits _) = FloatType $ LiteralParam bits
typeOfValue (AngleValue bits _) = AngleType $ LiteralParam bits
typeOfValue (BoolValue _) = BoolType
typeOfValue (DurationValue stretch _) = DurationType $ LiteralParam stretch
typeOfValue (ComplexValue bits realPart imagPart) = ComplexType $ LiteralParam bits

-- typeOfValue (ArrayValue elT dims _) = ArrayType {elementType = baseType, sizes = sizes}

{-
arccos   | float on [-1, 1] -> float on [0, pi]                                         | Inverse cosine.
arcsin   | float on [-1, 1] -> float on [-pi / 2, pi / 2]                               | Inverse sine.
arctan   | float -> float on [-pi / 2, pi / 2]                                          | Inverse tangent.
ceiling  | float -> float                                                               | Round to the nearest representable integer equal or greater in value.
cos      | (float or angle) -> float                                                    | Cosine.
exp      | float -> float; complex -> complex                                           | Exponential e ^x.
floor    | float -> float                                                               | Round to the nearest representable integer equal or lesser in value.
log      | float -> float                                                               | Logarithm base e.
mod      | int, int -> int; float, float -> float                                       | Modulus. The remainder from the integer division of the first argument by the second argument.
popcount | bit[_] -> uint                                                               | Number of set (1) bits.
pow      | int, uint -> int; float, float -> float; complex, complex -> complex         | pow(a, b) = a ^b. For floating-point and complex values, the principal value is returned.
rotl     | bit[n] value, int distance -> bit[n]; uint[n] value, int distance -> uint[n] | Rotate the bits in the representation of value by distance places to the left (towards higher indices). This is similar to a bit shift operation, except the vacated bits are filled from the overflow, rather than being set to zero. The width of the output is set equal to the width of the input. rotl(a, n) == rotr(a, -n).
rotr     | bit[n] value, int distance -> bit[n]; uint[n] value, int distance -> uint[n] | Rotate the bits in the representation of value by distance places to the right (towards lower indices).
sin      | (float or angle) -> float                                                    | Sine.
sqrt     | float -> float; complex -> complex                                           | Square root. This always returns the principal root.
tan      | (float or angle) -> float                                                    | Tangent.
-}
