module Qasm3.SemanticGraph where

import Ast qualified
import Control.Monad
import Control.Monad.State (runState)
import Control.Monad.State qualified as State
import Data.Map.Strict qualified as Map
import Data.Maybe
import Debug.Trace
import Qasm3.Syntax (Tag)

semanticGraphFrom :: Ast.Node Tag c -> SemanticGraph
semanticGraphFrom Ast.NilNode = initialSemanticGraph
semanticGraphFrom rootNode = initialSemanticGraph

data Ref = NilRef | Ref Int deriving (Eq, Ord, Read, Show)

data IdentifierAttributes = IdentifierAttributes
  { identName :: String,
    identRef :: Ref,
    identScopeRef :: Ref
    -- identType :: ExpressionType,
    -- identValue :: ConstantValue
  }
  deriving (Eq, Read, Show)

initialIdentifierAttributes :: IdentifierAttributes
initialIdentifierAttributes =
  IdentifierAttributes
    { identName = "",
      identRef = NilRef,
      identScopeRef = NilRef
      -- identType = NilType,
      -- identValue = NilValue
    }

-- data TypeParameter a = LiteralParam a | VariableParam Ref
--   deriving (Eq, Ord, Read, Show)

-- data ConstantValue
--   = NilValue
--   | IntValue Int
--   | TupleValue [ConstantValue]
--   deriving (Eq, Read, Show)

-- expressionTypeFromNode :: p -> ExpressionType
-- expressionTypeFromNode node = NilType -- TODO

data SemanticGraph = SemanticGraph
  { semGraphScopes :: Map.Map Ref LexicalScope,
    semGraphIdentifiers :: Map.Map Ref IdentifierAttributes
    -- semGraphExpressions :: Map.Map Ref SemanticNode,
    -- semGraphTypes :: Map.Map Ref ExpressionType,
    -- semGraphValues :: Map.Map Ref ConstantValue,
  }
  deriving (Eq, Read, Show)

initialSemanticGraph :: SemanticGraph
initialSemanticGraph =
  SemanticGraph
    { semGraphScopes = Map.empty,
      semGraphIdentifiers = Map.empty
      -- semGraphExpressions = Map.empty,
      -- semGraphTypes = Map.empty,
      -- semGraphValues = Map.empty
    }

data LexicalScope = LexicalScope
  { lexScopeParentRef :: Ref,
    lexScopeIdentifiers :: Map.Map String IdentifierAttributes
  }
  deriving (Eq, Read, Show)

initialLexicalScope :: LexicalScope
initialLexicalScope = LexicalScope NilRef Map.empty

newtype Program = Program Block deriving (Eq, Read, Show)

data Block = Block {-scope-} Ref {-stmts-} [Statement] deriving (Eq, Read, Show)

data Statement
  = AliasDeclarationStmt ExpressionType Identifier {-rvalue|bitconcat-} [Expression]
  | ConstDeclarationStmt ExpressionType Identifier {-const-} Expression
  | ClassicalDeclarationStmt IoModifier {-ctype-} ExpressionType Identifier Expression
  | QuantumDeclarationStmt {-qtype-} ExpressionType Identifier
  | AssignmentStmt {-lvalue-} Expression Expression
  | CompoundAssignmentStmt AssignmentOperator {-lvalue-} Expression Expression
  | GateCallStmt {-ctype-} Identifier [Expression {-ctype-}] ExpressionType -- [modifiers::List<GateModifier>, target::Identifier, params::List<Expression>?, designator::Expression?, args::List<(HardwareQubit | IndexedIdentifier)>?]
  | ResetStmt {-qvalue-} Expression
  | BarrierStmt {-qvalue-} [Expression]
  | DelayStmt {-tvalue-} Expression {-qvalue-} Expression
  | BoxStmt Identifier Block
  | BreakStmt
  | ContinueStmt
  | EndStmt
  | ReturnStmt {-rvalue-} Expression
  | ExpressionStmt Expression
  | IfStmt {-rvalue-} Expression Block Block -- [condition::Expression, thenBlock::(Statement | Scope), elseBlock::(Statement | Scope)?
  | ForStmt
      {-ctype-} ExpressionType
      Identifier
      {-rvalue-} Expression
      Block
  | WhileStmt {-rvalue-} Expression Block
  deriving (Eq, Read, Show)

data Identifier = Identifier Ref String deriving (Eq, Read, Show)

data Expression
  = IdentifierExpr ExpressionType Ref
  | IndexExpr {-rvalue|qvalue-} Expression {-bitvalue|arrayvalue|rangevalue|setvalue-} Expression
  | UnaryOperatorExpr UnaryOperator {-rvalue-} Expression
  | BinaryOperatorExpr
      BinaryOperator
      {-rvalue-} Expression
      {-rvalue-} Expression
  | CastExpr {-ctype-} ExpressionType {-rvalue-} Expression
  | DurationOfExpr Block
  | CallExpr Ref {-rvalue-} [Expression]
  | ArrayInitExpr {-rvalue|arrayvalue-} [Expression]
  | SetInitExpr {-rvalue-} [Expression]
  | RangeInitExpr
      {-rvalue-} Expression
      {-rvalue?-} Expression
      {-rvalue-} Expression
  | MeasureExpr {-qvalue-} [Expression]
  deriving (Eq, Read, Show)

data ExpressionType
  = NilType
  | BitType Int -- size
  | IntType Int -- size
  | UintType Int -- size
  | FloatType Int -- size
  | AngleType Int -- size
  | BoolType
  | DurationType
  | StretchType
  | ComplexType ExpressionType -- base
  | QubitType Int -- size
  | HwQubitType Int -- hwindex
  | ArrayType ExpressionType Int -- base, size (= -1 if unspecified)
  | ArrayRefType ExpressionType Bool -- base, mutable
  deriving (Eq, Read, Show)

data AssignmentOperator
  = PlusAsgn
  | MinusAsgn
  | AsteriskAsgn
  | SlashAsgn
  | AmpersandAsgn
  | PipeAsgn
  | TildeAsgn
  | CaretAsgn
  | DoubleLessAsgn
  | DoubleGreaterAsgn
  | PercentAsgn
  | DoubleAsteriskAsgn
  deriving (Eq, Read, Show)

data BinaryOperator
  = PlusBinop
  | DoublePlusBinop
  | MinusBinop
  | AsteriskBinop
  | DoubleAsteriskBinop
  | SlashBinop
  | PercentBinop
  | PipeBinop
  | DoublePipeBinop
  | AmpersandBinop
  | DoubleAmpersandBinop
  | CaretBinop
  | DoubleLessBinop
  | DoubleGreaterBinop
  | DoubleEqualsBinop
  | ExclamationPointEqualsBinop
  | LessBinop
  | GreaterBinop
  | LessEqualsBinop
  | GreaterEqualsBinop
  deriving (Eq, Read, Show)

data UnaryOperator
  = MinusUnop
  | TildeUnop
  | ExclamationPointUnop
  deriving (Eq, Read, Show)

data IoModifier
  = NilMod
  | InputMod
  | OutputMod
  deriving (Eq, Read, Show)

{-
data ScalarType
  = BitType {scalarBitSize :: TypeParameter Int}
  | IntType {scalarBitSize :: TypeParameter Int}
  | UintType {scalarBitSize :: TypeParameter Int}
  | FloatType {scalarBitSize :: TypeParameter Int}
  | AngleType {scalarBitSize :: TypeParameter Int}
  | BoolType
  | DurationType {scalarStretch :: TypeParameter Bool}
  | ComplexType {scalarBitSize :: TypeParameter Int}
  deriving (Eq, Read, Show)

data ExpressionType
  = NilExpr
  | ScalarExpr {exprScalarType :: TypeParameter ScalarType}
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

-}