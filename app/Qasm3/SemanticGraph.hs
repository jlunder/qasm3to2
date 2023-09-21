module Qasm3.SemanticGraph where

import Ast qualified
import Control.Monad
import Control.Monad.State (runState)
import Control.Monad.State qualified as State
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Word (Word64)
import Debug.Trace
import Qasm3.Syntax qualified as Q3

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
  { semGraphProgram :: Program,
    semGraphScopes :: Map.Map Ref LexicalScope,
    semGraphIdentifiers :: Map.Map Ref IdentifierAttributes
    -- semGraphExpressions :: Map.Map Ref SemanticNode,
    -- semGraphTypes :: Map.Map Ref ExpressionType,
    -- semGraphValues :: Map.Map Ref ConstantValue,
  }
  deriving (Eq, Read, Show)

initialSemanticGraph :: SemanticGraph
initialSemanticGraph =
  SemanticGraph
    { semGraphProgram = Program [],
      semGraphScopes = Map.empty,
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

newtype Program = Program [Statement] deriving (Eq, Read, Show)

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
  | BoxStmt Identifier [Statement]
  | BreakStmt
  | ContinueStmt
  | EndStmt
  | ReturnStmt {-rvalue-} Expression
  | ExpressionStmt Expression
  | IfStmt {-rvalue-} Expression [Statement] [Statement] -- [condition::Expression, thenBlock::(Statement | Scope), elseBlock::(Statement | Scope)?
  | ForStmt
      {-ctype-} ExpressionType
      Identifier
      {-rvalue-} Expression
      [Statement]
  | WhileStmt {-rvalue-} Expression [Statement]
  deriving (Eq, Read, Show)

data Identifier = Identifier Ref String deriving (Eq, Read, Show)

data Expression
  = NilExpr
  | IdentifierExpr Ref
  | LiteralExpr ConstantValue
  | IndexExpr {-rvalue|qvalue-} Expression {-bitvalue|arrayvalue|rangevalue|setvalue-} Expression
  | UnaryOperatorExpr UnaryOperator {-rvalue-} Expression
  | BinaryOperatorExpr
      BinaryOperator
      {-rvalue-} Expression
      {-rvalue-} Expression
  | CastExpr {-ctype-} ExpressionType {-rvalue-} Expression
  | DurationOfExpr [Statement]
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
  | BitType ConstantValue -- size
  | IntType ConstantValue -- size
  | UintType ConstantValue -- size
  | FloatType ConstantValue -- size
  | AngleType ConstantValue -- size
  | BoolType
  | DurationType
  | StretchType
  | ComplexType ExpressionType -- base
  | QubitType ConstantValue -- size
  | HwQubitType ConstantValue -- hwindex
  | ArrayType ExpressionType ConstantValue -- base, size (= -1 if unspecified)
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
  = NilIoMod
  | InputIoMod
  | OutputIoMod
  deriving (Eq, Read, Show)

data GateModifier
  = NilGateMod
  | InvGateMod
  | PowGateMod Expression
  | CtrlGateMod Expression
  | NegCtrlGateMod Expression
  deriving (Eq, Read, Show)

data ConstantValue
  = NilValue
  | BitValue Int Int64
  | IntValue Int Int64
  | UintValue Int Word64
  | FloatValue Int Double
  | AngleValue Int Double
  | BoolValue Bool
  | DurationValue Bool Double
  | ComplexValue Int Double Double
  | ArrayValue [ConstantValue]
  deriving (Eq, Read, Show)

{-
-- Reducing duration expressions is nontrivial
data Duration
  = ZeroDuration
  | ExactDtDuration Double
  | ExactNsDuration Double
  | StretchDuration
  | SumDuration [Duration]
  | MaxDuration [Duration]
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

semanticGraphFrom :: Q3.SyntaxNode -> SemanticGraph
semanticGraphFrom (Ast.Node (Q3.Program _ _ tok) stmts _) =
  initialSemanticGraph {semGraphProgram = Program $ semanticGraphStatementsFrom stmts}

semanticGraphStatementsFrom :: [Q3.SyntaxNode] -> [Statement]
semanticGraphStatementsFrom = mapMaybe semanticGraphStatementFrom

semanticGraphStatementFrom :: Q3.SyntaxNode -> Maybe Statement
semanticGraphStatementFrom (Ast.Node (Q3.Pragma ctnt _) [] _) = Nothing
semanticGraphStatementFrom (Ast.Node Q3.Statement (stmt : annots) _) = semanticGraphStatementFrom stmt
semanticGraphStatementFrom (Ast.Node (Q3.Annotation name ctnt _) [] _) = Nothing
semanticGraphStatementFrom (Ast.Node Q3.AliasDeclStmt (ident : exprs) _) = Nothing
semanticGraphStatementFrom (Ast.Node (Q3.AssignmentStmt op) [target, expr] _) = Nothing -- TODO
semanticGraphStatementFrom (Ast.Node Q3.BarrierStmt gateOperands _) = Nothing -- TODO
semanticGraphStatementFrom (Ast.Node Q3.BoxStmt [time, stmts] _) = Nothing -- TODO
semanticGraphStatementFrom (Ast.Node Q3.BreakStmt [] _) = Just BreakStmt
semanticGraphStatementFrom (Ast.Node (Q3.CalStmt calBlock) [] _) = Nothing
semanticGraphStatementFrom (Ast.Node (Q3.DefcalgrammarStmt _ cgname) [] _) = Nothing
semanticGraphStatementFrom (Ast.Node Q3.ClassicalDeclStmt [anyType, ident, maybeExpr] _) = Nothing -- TODO
semanticGraphStatementFrom (Ast.Node Q3.ConstDeclStmt [sclrType, ident, maybeExpr] _) = Nothing -- TODO
semanticGraphStatementFrom (Ast.Node Q3.ContinueStmt [] _) = Just ContinueStmt
semanticGraphStatementFrom (Ast.Node Q3.DefStmt [ident, argDefs, returnType, stmts] _) = Nothing -- TODO
semanticGraphStatementFrom (Ast.Node Q3.DelayStmt (designator : gateOperands) _) = Nothing -- TODO
semanticGraphStatementFrom (Ast.Node Q3.DefcalStmt [defcalTarget, defcalArgs, defcalOps, returnType, calBlock] _) =
  Nothing
semanticGraphStatementFrom (Ast.Node Q3.EndStmt [] _) = Just EndStmt
semanticGraphStatementFrom (Ast.Node Q3.ExpressionStmt [expr] _) =
  Just $ ExpressionStmt $ semanticGraphExpressionFrom expr
semanticGraphStatementFrom (Ast.Node Q3.ExternStmt [ident, paramTypes, returnType] _) = Nothing
semanticGraphStatementFrom (Ast.Node Q3.ForStmt [anyType, ident, loopExpr, loopStmt] _) =
  Just $
    ForStmt
      (semanticGraphExpressionTypeFrom anyType)
      (semanticGraphIdentifierFrom ident)
      (semanticGraphExpressionFrom loopExpr)
      (catMaybes $ semanticGraphBlockFrom loopStmt)
semanticGraphStatementFrom (Ast.Node Q3.GateStmt [ident, params, args, stmts] _) = Nothing -- TODO
semanticGraphStatementFrom (Ast.Node Q3.GateCallStmt [modifiers, target, params, maybeTime, gateArgs] _) = Nothing -- TODO
semanticGraphStatementFrom (Ast.Node Q3.IfStmt [condExpr, thenBlock, maybeElseBlock] _) =
  Just $
    IfStmt
      (semanticGraphExpressionFrom condExpr)
      (catMaybes $ semanticGraphBlockFrom thenBlock)
      (catMaybes $ semanticGraphBlockFrom maybeElseBlock)
semanticGraphStatementFrom (Ast.Node (Q3.IncludeStmt _ tok) [] _) =
  trace "includes must be resolved before handling by SemanticGraph" undefined
semanticGraphStatementFrom (Ast.Node Q3.InputIoDeclStmt [anyType, ident] _) = Nothing -- TODO
semanticGraphStatementFrom (Ast.Node Q3.OutputIoDeclStmt [anyType, ident] _) = Nothing -- TODO
semanticGraphStatementFrom (Ast.Node Q3.MeasureArrowAssignmentStmt [msrExpr, maybeTgt] _) = Nothing -- TODO
semanticGraphStatementFrom (Ast.Node Q3.CregOldStyleDeclStmt [ident, maybeSize] _) =
  Just $
    ClassicalDeclarationStmt
      NilIoMod
      (BitType $ semanticGraphConstantValueFrom maybeSize)
      (semanticGraphIdentifierFrom ident)
      NilExpr
semanticGraphStatementFrom (Ast.Node Q3.QregOldStyleDeclStmt [ident, maybeSize] _) =
  Just $
    QuantumDeclarationStmt
      (QubitType $ semanticGraphConstantValueFrom maybeSize)
      (semanticGraphIdentifierFrom ident)
semanticGraphStatementFrom (Ast.Node Q3.QuantumDeclStmt [qubitType, ident] _) =
  Just $ QuantumDeclarationStmt (semanticGraphExpressionTypeFrom qubitType) (semanticGraphIdentifierFrom ident)
semanticGraphStatementFrom (Ast.Node Q3.ResetStmt [gateOp] _) = Nothing -- TODO
semanticGraphStatementFrom (Ast.Node Q3.ReturnStmt [maybeExpr] _) =
  Just $ ReturnStmt $ semanticGraphExpressionFrom maybeExpr
semanticGraphStatementFrom (Ast.Node Q3.WhileStmt [condExpr, loopBlock] _) =
  Just $ WhileStmt (semanticGraphExpressionFrom condExpr) (catMaybes $ semanticGraphBlockFrom loopBlock)
semanticGraphStatementFrom node = trace ("Missing pattern for semanticGraphStatementFrom: " ++ show node) undefined

semanticGraphBlockFrom :: Q3.SyntaxNode -> [Maybe Statement]
semanticGraphBlockFrom (Ast.Node Q3.Scope stmts _) = map semanticGraphStatementFrom stmts
semanticGraphBlockFrom stmt = [semanticGraphStatementFrom stmt]

semanticGraphIdentifierFrom :: Q3.SyntaxNode -> Identifier
semanticGraphIdentifierFrom ident = Identifier NilRef "" -- TODO

semanticGraphConstantValueFrom :: Q3.SyntaxNode -> ConstantValue
semanticGraphConstantValueFrom constVal = NilValue -- TODO

semanticGraphExpressionFrom :: Q3.SyntaxNode -> Expression
semanticGraphExpressionFrom (Ast.Node Q3.ParenExpr [expr] _) = semanticGraphExpressionFrom expr
semanticGraphExpressionFrom (Ast.Node Q3.IndexExpr [expr, index] _) = NilExpr
semanticGraphExpressionFrom (Ast.Node (Q3.UnaryOperatorExpr op) [expr] _) = NilExpr -- TODO
semanticGraphExpressionFrom (Ast.Node (Q3.BinaryOperatorExpr op) [left, right] _) = NilExpr -- TODO
semanticGraphExpressionFrom (Ast.Node Q3.CastExpr [anyType, expr] _) = NilExpr -- TODO
semanticGraphExpressionFrom (Ast.Node Q3.DurationOfExpr stmts _) = NilExpr -- TODO
semanticGraphExpressionFrom (Ast.Node Q3.CallExpr (ident : exprs) _) = NilExpr -- TODO
semanticGraphExpressionFrom (Ast.Node (Q3.Identifier _ tok) [] _) = NilExpr -- TODO
semanticGraphExpressionFrom (Ast.Node (Q3.IntegerLiteral _ tok) [] _) = NilExpr -- TODO
semanticGraphExpressionFrom (Ast.Node (Q3.FloatLiteral _ tok) [] _) = NilExpr -- TODO
semanticGraphExpressionFrom (Ast.Node (Q3.ImaginaryLiteral _ tok) [] _) = NilExpr -- TODO
semanticGraphExpressionFrom (Ast.Node (Q3.BooleanLiteral _ tok) [] _) = NilExpr -- TODO
semanticGraphExpressionFrom (Ast.Node (Q3.BitstringLiteral _ tok) [] _) = NilExpr -- TODO
semanticGraphExpressionFrom (Ast.Node (Q3.TimingLiteral _ tok) [] _) = NilExpr -- TODO
semanticGraphExpressionFrom (Ast.Node (Q3.HardwareQubit _ tok) [] _) = NilExpr -- TODO
semanticGraphExpressionFrom (Ast.Node Q3.ArrayInitExpr elems _) = NilExpr -- TODO
semanticGraphExpressionFrom (Ast.Node Q3.SetInitExpr elems _) = NilExpr -- TODO
semanticGraphExpressionFrom (Ast.Node Q3.RangeInitExpr [begin, step, end] _) = NilExpr -- TODO
semanticGraphExpressionFrom (Ast.Node Q3.DimExpr [size] _) = NilExpr -- TODO
semanticGraphExpressionFrom (Ast.Node Q3.MeasureExpr [gateOp] _) = NilExpr -- TODO
semanticGraphExpressionFrom (Ast.Node Q3.IndexedIdentifier (ident : indices) _) = NilExpr -- TODO

semanticGraphGateModifierFrom :: Q3.SyntaxNode -> GateModifier
semanticGraphGateModifierFrom (Ast.Node Q3.InvGateModifier [] _) = InvGateMod
semanticGraphGateModifierFrom (Ast.Node Q3.PowGateModifier [expr] _) = PowGateMod (semanticGraphExpressionFrom expr)
semanticGraphGateModifierFrom (Ast.Node Q3.CtrlGateModifier [maybeExpr] _) =
  CtrlGateMod (semanticGraphExpressionFrom maybeExpr)
semanticGraphGateModifierFrom (Ast.Node Q3.NegCtrlGateModifier [maybeExpr] _) =
  NegCtrlGateMod (semanticGraphExpressionFrom maybeExpr)

semanticGraphExpressionTypeFrom :: Q3.SyntaxNode -> ExpressionType
semanticGraphExpressionTypeFrom (Ast.Node Q3.BitTypeSpec [maybeSize] _) = NilType -- TODO
semanticGraphExpressionTypeFrom (Ast.Node Q3.CregTypeSpec [maybeSize] _) = NilType -- TODO
semanticGraphExpressionTypeFrom (Ast.Node Q3.QregTypeSpec [maybeSize] _) = NilType -- TODO
semanticGraphExpressionTypeFrom (Ast.Node Q3.IntTypeSpec [maybeSize] _) = NilType -- TODO
semanticGraphExpressionTypeFrom (Ast.Node Q3.UintTypeSpec [maybeSize] _) = NilType -- TODO
semanticGraphExpressionTypeFrom (Ast.Node Q3.FloatTypeSpec [maybeSize] _) = NilType -- TODO
semanticGraphExpressionTypeFrom (Ast.Node Q3.AngleTypeSpec [maybeSize] _) = NilType -- TODO
semanticGraphExpressionTypeFrom (Ast.Node Q3.BoolTypeSpec [] _) = NilType -- TODO
semanticGraphExpressionTypeFrom (Ast.Node Q3.DurationTypeSpec [] _) = NilType -- TODO
semanticGraphExpressionTypeFrom (Ast.Node Q3.StretchTypeSpec [] _) = NilType -- TODO
semanticGraphExpressionTypeFrom (Ast.Node Q3.ComplexTypeSpec [maybeSclr] _) = NilType -- TODO
semanticGraphExpressionTypeFrom (Ast.Node Q3.QubitTypeSpec [maybeSize] _) = NilType -- TODO
semanticGraphExpressionTypeFrom (Ast.Node Q3.ArrayTypeSpec (sclrType : exprs) _) = NilType -- TODO
semanticGraphExpressionTypeFrom (Ast.Node Q3.ReadonlyArrayRefTypeSpec (sclrType : exprs) _) = NilType -- TODO
semanticGraphExpressionTypeFrom (Ast.Node Q3.MutableArrayRefTypeSpec (sclrType : exprs) _) = NilType -- TODO

{-
semanticGraphFrom (Ast.Node (Q3.DefcalTarget tgt _) [] _) = undefined
semanticGraphFrom (Ast.Node Q3.ArgumentDefinition [anyType, ident] _) = undefined
{- Error cases -}
-- Should have been handled above -- usually implies some change to how the surrounding renders
semanticGraphFrom Ast.NilNode = trace "Unhandled NilNode for semanticGraphFrom" undefined
-- Should have been handled above -- we can't know which separator to use
semanticGraphFrom (Ast.Node Q3.List elems _) = trace ("Unhandled List node for semanticGraphFrom with children: " ++ show elems) undefined
-- Fallback
semanticGraphFrom node = trace ("Missing pattern for semanticGraphFrom: " ++ show node) undefined
-}

