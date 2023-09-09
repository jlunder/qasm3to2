module Qasm3SemanticAnalyzer where

import Ast
import Control.Monad.State qualified as State
import Data.Int (Int64)
import Data.Map.Lazy qualified as Map
import Data.Word (Word64)
import Qasm3

type Ref = Int

data TypeParameter a = LiteralParam a | VariableParam Ref

type SemanticNode = AstNode Tag SemanticAnnotation

data SemanticAnnotation = SemanticAnnotation
  { expressionAttributes :: ExpressionAttributes,
    definingScope :: Maybe Ref,
    referencingScopes :: [Ref]
    -- reducedExpression :: Expression
  }

data SemanticState = SemanticState
  { allIdentifiers :: Map.Map Int IdentifierAttributes,
    allScopes :: Map.Map Int LexicalScope,
    typeVariables :: Map.Map Int Int,
    nextRefId :: Int
  }

data IdentifierAttributes = IdentifierAttributes
  { name :: String
  }

data LexicalScope = LexicalScope
  { identifiers :: Map.Map String Int,
    parentScope :: Int
  }

newtype IdentifierRef a = Int a

type SemanticStateM a = State.State SemanticState a

allocateRefId :: SemanticStateM Int
allocateRefId = do
  state <- State.get
  let refId = nextRefId state
  State.put state {nextRefId = refId + 1}
  return refId

-- Function to update the state for an Identifier node
updateStateForIdentifier :: String -> SemanticStateM ()
updateStateForIdentifier identifier = do
  refId <- allocateRefId
  state <- State.get
  -- find identifier in scope
  -- already there? match to existing one
  -- missing? make a new one
  let updatedIdentifiers = Map.insert refId (IdentifierAttributes identifier) (allIdentifiers state)
  State.put (state {allIdentifiers = updatedIdentifiers})

-- Recursive function to traverse the tree and update state
traverseAst :: AstNode Tag c -> SemanticStateM ()
traverseAst NilNode = return ()
traverseAst (AstNode tag children _) = do
  case tag of
    Identifier identifier _ -> updateStateForIdentifier identifier
    _ -> return ()
  mapM_ traverseAst children

data ScalarType
  = ErrorType
  | BitType {bitSize :: TypeParameter Int}
  | IntType {bitSize :: TypeParameter Int}
  | UintType {bitSize :: TypeParameter Int}
  | FloatType {bitSize :: TypeParameter Int}
  | AngleType {bitSize :: TypeParameter Int}
  | BoolType
  | DurationType {stretch :: Bool}
  | ComplexType {bitSize :: TypeParameter Int}

data ExpressionAttributes
  = ScalarExpr {exprType :: ScalarType}
  | QubitExpr {eaBits :: TypeParameter Int}
  | HardwareQubitExpr
  | IndexExpr {}
  | ArrayExpr {mutable :: Bool, elementType :: ScalarType, dimensions :: [TypeParameter Int]}

-- Reducing duration expressions is nontrivial
data Duration
  = ZeroDuration
  | ExactDtDuration Double
  | ExactNsDuration Double
  | StretchDuration
  | SumDuration [Duration]
  | MaxDuration [Duration]

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

-- coerceToCompatible
-- coerceToIntegral

typeOfValue (BitValue bits _) = BitType $ LiteralParam bits
typeOfValue (IntValue bits _) = IntType $ LiteralParam bits
typeOfValue (UintValue bits _) = UintType $ LiteralParam bits
typeOfValue (FloatValue bits _) = FloatType $ LiteralParam bits
typeOfValue (AngleValue bits _) = AngleType $ LiteralParam bits
typeOfValue (BoolValue _) = BoolType
typeOfValue (DurationValue stretch _) = DurationType stretch
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
