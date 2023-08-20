{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Qasm3 where

import Ast

data Token
  = EofToken
  | OpenqasmToken
  | IncludeToken
  | DefcalgrammarToken
  | DefToken
  | CalToken
  | DefcalToken
  | GateToken
  | ExternToken
  | BoxToken
  | LetToken
  | BreakToken
  | ContinueToken
  | IfToken
  | ElseToken
  | EndToken
  | ReturnToken
  | ForToken
  | WhileToken
  | InToken
  | PragmaToken
  | AnnotationKeywordToken String
  | InputToken
  | OutputToken
  | ConstToken
  | ReadonlyToken
  | MutableToken
  | QregToken
  | QubitToken
  | CregToken
  | BoolToken
  | BitToken
  | IntToken
  | UintToken
  | FloatToken
  | AngleToken
  | ComplexToken
  | ArrayToken
  | VoidToken
  | DurationToken
  | StretchToken
  | GphaseToken
  | InvToken
  | PowToken
  | CtrlToken
  | NegctrlToken
  | DimToken
  | DurationofToken
  | DelayToken
  | ResetToken
  | MeasureToken
  | BarrierToken
  | BooleanLiteralToken String
  | LbracketToken
  | RbracketToken
  | LbraceToken
  | RbraceToken
  | LparenToken
  | RparenToken
  | ColonToken
  | SemicolonToken
  | DotToken
  | CommaToken
  | EqualsToken
  | ArrowToken
  | PlusToken
  | DoublePlusToken
  | MinusToken
  | AsteriskToken
  | DoubleAsteriskToken
  | SlashToken
  | PercentToken
  | PipeToken
  | DoublePipeToken
  | AmpersandToken
  | DoubleAmpersandToken
  | CaretToken
  | AtToken
  | TildeToken
  | ExclamationPointToken
  | EqualityOperatorToken String
  | CompoundAssignmentOperatorToken String
  | ComparisonOperatorToken String
  | BitshiftOperatorToken String
  | ImagToken
  | ImaginaryLiteralToken String
  | BinaryIntegerLiteralToken String
  | OctalIntegerLiteralToken String
  | DecimalIntegerLiteralToken String
  | HexIntegerLiteralToken String
  | IdentifierToken String
  | HardwareQubitToken String
  | FloatLiteralToken String
  | TimingLiteralToken String
  | BitstringLiteralToken String
  | WhitespaceToken String
  | NewlineToken String
  | LineCommentToken String
  | BlockCommentToken String
  | VersionIdentiferWhitespaceToken
  | VersionSpecifierToken String
  | ArbitraryStringWhitespaceToken
  | StringLiteralToken String
  | EatInitialSpaceToken
  | EatLineEndToken
  | RemainingLineContentToken String
  | CalPreludeWhitespaceToken
  | CalPreludeCommentToken
  | CalPreludeLbraceToken
  | DefcalPreludeWhitespaceToken
  | DefcalPreludeCommentToken
  | DefcalPreludeLbraceToken
  | DefcalPreludeQregToken
  | DefcalPreludeQubitToken
  | DefcalPreludeCregToken
  | DefcalPreludeBoolToken
  | DefcalPreludeBitToken
  | DefcalPreludeIntToken
  | DefcalPreludeUintToken
  | DefcalPreludeAngleToken
  | DefcalPreludeFloatToken
  | DefcalPreludeComplexToken
  | DefcalPreludeArrayToken
  | DefcalPreludeDurationToken
  | DefcalPreludeLbracketToken
  | DefcalPreludeRbracketToken
  | DefcalPreludeLparenToken
  | DefcalPreludeRparenToken
  | DefcalPreludeArrowToken
  | DefcalPreludeCommaToken
  | DefcalPreludePlusToken
  | DefcalPreludeMinusToken
  | DefcalPreludeAsteriskToken
  | DefcalPreludeSlashToken
  | DefcalPreludeBitshiftOperatorToken String
  | DefcalPreludeBitstringLiteralToken String
  | DefcalPreludeBinaryIntegerLiteralToken String
  | DefcalPreludeOctalIntegerLiteralToken String
  | DefcalPreludeDecimalIntegerLiteralToken String
  | DefcalPreludeHexIntegerLiteralToken String
  | DefcalPreludeFloatLiteralToken String
  | DefcalPreludeMeasureToken
  | DefcalPreludeDelayToken
  | DefcalPreludeResetToken
  | DefcalPreludeIdentifierToken String
  | DefcalPreludeHardwareQubitToken String
  | CalibrationBlockToken String
  | CalBlockRbraceToken
  deriving (Eq, Ord, Read, Show)

data ProgramNode = Program VersionSpecifierNode [StatementNode]
  deriving (Eq, Read, Show)

data StatementNode
  = Pragma RemainingLineContentNode
  | Annotated [AnnotationNode] StatementContentNode
  deriving (Eq, Read, Show)

data AnnotationNode = Annotation AnnotationKeywordNode RemainingLineContentNode
  deriving (Eq, Read, Show)

data StatementContentNode
  = AliasDeclaration IdentifierNode AliasExpressionNode
  | Assignment IndexedIdentifierNode CompoundAssignmentOperatorNode MeasureExpressionNode
  | Barrier [GateOperandNode]
  | Box (Maybe DesignatorNode)
  | Break
  | Cal (Maybe CalibrationBlockNode)
  | CalibrationGrammar StringLiteralNode
  | ClassicalDeclaration ScalarOrArrayTypeNode IdentifierNode (Maybe DeclarationExpressionNode)
  | ConstDeclaration ScalarTypeNode IdentifierNode DeclarationExpressionNode
  | Continue
  | Def IdentifierNode [ArgumentDefinitionNode] ScalarTypeNode
  | Defcal DefcalTargetNode [DefcalArgumentDefinitionNode] [DefcalOperandNode] (Maybe ScalarTypeNode) (Maybe CalibrationBlockNode)
  | Delay DesignatorNode [GateOperandNode]
  | End
  | Expression ExpressionNode
  | Extern IdentifierNode [ArgumentDefinitionNode] ScalarTypeNode
  | For ScalarTypeNode IdentifierNode ExpressionNode StatementOrScopeNode
  | RangeFor ScalarTypeNode IdentifierNode RangeExpressionNode StatementOrScopeNode
  | SetFor ScalarTypeNode IdentifierNode SetExpressionNode StatementOrScopeNode
  | Gate IdentifierNode [IdentifierNode] [IdentifierNode]
  | GateCall [GateModifierNode] IdentifierNode [ExpressionNode] (Maybe DesignatorNode) [GateOperandNode]
  | If ExpressionNode StatementOrScopeNode (Maybe StatementOrScopeNode)
  | Include StringLiteralNode
  | InputIoDeclaration ScalarOrArrayTypeNode IdentifierNode
  | OutputIoDeclaration ScalarOrArrayTypeNode IdentifierNode
  | MeasureArrowAssignment GateOperandNode (Maybe IndexedIdentifierNode)
  | CregOldStyleDeclaration IdentifierNode (Maybe DesignatorNode)
  | QregOldStyleDeclaration IdentifierNode (Maybe DesignatorNode)
  | QuantumDeclaration QubitTypeNode IdentifierNode
  | Reset GateOperandNode
  | Return MeasureExpressionNode
  | While ExpressionNode StatementOrScopeNode
  deriving (Eq, Read, Show)

data ScalarOrArrayTypeNode = Scalar ScalarTypeNode | Array ArrayTypeNode
  deriving (Eq, Read, Show)

data StatementOrScopeNode = Statement StatementNode | Scope [StatementNode]
  deriving (Eq, Read, Show)

newtype AliasExpressionNode = AliasExpression [ExpressionNode]
  deriving (Eq, Read, Show)

data ExpressionNode
  = ParenExpression ExpressionNode
  | IndexExpression ExpressionNode IndexOperatorNode
  | UnaryExpression Token ExpressionNode
  | BinaryExpression ExpressionNode Token ExpressionNode
  | CastExpression ScalarOrArrayTypeNode ExpressionNode
  | CallExpression IdentifierNode [ExpressionNode]
  | BinaryIntegerExpression BinaryIntegerLiteralNode
  | OctalIntegerExpression OctalIntegerLiteralNode
  | DecimalIntegerExpression DecimalIntegerLiteralNode
  | HexIntegerExpression HexIntegerLiteralNode
  | FloatExpression FloatLiteralNode
  | ImaginaryExpression ImaginaryLiteralNode
  | BooleanExpression BooleanLiteralNode
  | BitstringExpression BitstringLiteralNode
  | TimingExpression TimingLiteralNode
  | HardwareQubitExpression HardwareQubitNode
  deriving (Eq, Read, Show)

data GateModifierNode
  = InvGateModifier
  | PowGateModifier ExpressionNode
  | CtrlGateModifier (Maybe ExpressionNode)
  | NegCtrlGateModifier (Maybe ExpressionNode)
  deriving (Eq, Read, Show)

data ScalarTypeNode
  = BitType (Maybe DesignatorNode)
  | IntType (Maybe DesignatorNode)
  | UintType (Maybe DesignatorNode)
  | FloatType (Maybe DesignatorNode)
  | AngleType (Maybe DesignatorNode)
  | BoolType
  | DurationType
  | StretchType
  | ComplexType (Maybe ScalarTypeNode)
  deriving (Eq, Read, Show)

newtype QubitTypeNode = QubitType (Maybe DesignatorNode)
  deriving (Eq, Read, Show)

data ArrayTypeNode = ArrayType ScalarTypeNode [ExpressionNode]
  deriving (Eq, Read, Show)

data ArgumentDefinitionNode
  = ScalarArgument ScalarTypeNode IdentifierNode
  | QubitArgument QubitTypeNode IdentifierNode
  | CregArgument IdentifierNode (Maybe DesignatorNode)
  | QregArgument IdentifierNode (Maybe DesignatorNode)
  | ArrayArgument ArrayReferenceTypeNode IdentifierNode
  deriving (Eq, Read, Show)

data ArrayReferenceTypeNode
  = ReadonlyArrayReferenceType ScalarTypeNode [ExpressionNode]
  | MutableArrayReferenceType ScalarTypeNode [ExpressionNode]
  | ReadonlyArrayReferenceDimType ScalarTypeNode ExpressionNode
  | MutableArrayReferenceDimType ScalarTypeNode ExpressionNode
  deriving (Eq, Read, Show)

newtype DesignatorNode = Designator ExpressionNode
  deriving (Eq, Read, Show)

data DeclarationExpressionNode
  = ArrayLiteralDeclarationExpression ArrayLiteralNode
  | ExpressionDeclarationExpression MeasureExpressionNode
  deriving (Eq, Read, Show)

data DefcalTargetNode
  = MeasureDefcalTarget
  | ResetDefcalTarget
  | DelayDefcalTarget
  | IdentifierDefcalTarget IdentifierNode
  deriving (Eq, Read, Show)

data DefcalArgumentDefinitionNode
  = ExpressionDefcalArgument ExpressionNode
  | ArgumentDefinitionDefcalArgument ArgumentDefinitionNode
  deriving (Eq, Read, Show)

data DefcalOperandNode
  = HardwardDefcal HardwareQubitNode
  | IdentifierDefcal IdentifierNode
  deriving (Eq, Read, Show)

data ExternArgumentNode
  = ScalarExternArgument ScalarTypeNode
  | ArrayExternArgument ArrayReferenceTypeNode
  | CregExternArgument (Maybe DesignatorNode)
  deriving (Eq, Read, Show)

data GateOperandNode = IdentifierGateOperand IndexedIdentifierNode | QubitGateOperand HardwareQubitNode
  deriving (Eq, Read, Show)

data IndexedIdentifierNode = IndexedIdentifier IdentifierNode [IndexOperatorNode]
  deriving (Eq, Read, Show)

data IndexOperatorNode = SetIndex SetExpressionNode | IndexList [RangeOrExpressionIndex]
  deriving (Eq, Read, Show)

data RangeOrExpressionIndex = ExpressionIndex ExpressionNode | RangeIndex RangeExpressionNode
  deriving (Eq, Read, Show)

data MeasureExpressionNode
  = PlainExpression ExpressionNode
  | MeasureExpression GateOperandNode
  deriving (Eq, Read, Show)

data RangeExpressionNode = RangeExpression ExpressionNode ExpressionNode (Maybe ExpressionNode)
  deriving (Eq, Read, Show)

newtype SetExpressionNode = SetExpression [ExpressionNode]
  deriving (Eq, Read, Show)

newtype ArrayLiteralNode = ArrayLiteral [ArrayElementNode]
  deriving (Eq, Read, Show)

data ArrayElementNode = ExpressionArrayElement ExpressionNode | ArrayArrayElement ArrayLiteralNode
  deriving (Eq, Read, Show)

newtype AnnotationKeywordNode = AnnotationKeyword String
  deriving (Eq, Read, Show)

newtype BooleanLiteralNode = BooleanLiteral String
  deriving (Eq, Read, Show)

newtype EqualityOperatorNode = EqualityOperator String
  deriving (Eq, Read, Show)

newtype CompoundAssignmentOperatorNode = CompoundAssignmentOperator String
  deriving (Eq, Read, Show)

newtype ComparisonOperatorNode = ComparisonOperator String
  deriving (Eq, Read, Show)

newtype BitshiftOperatorNode = BitshiftOperator String
  deriving (Eq, Read, Show)

newtype ImaginaryLiteralNode = ImaginaryLiteral String
  deriving (Eq, Read, Show)

newtype BinaryIntegerLiteralNode = BinaryIntegerLiteral String
  deriving (Eq, Read, Show)

newtype OctalIntegerLiteralNode = OctalIntegerLiteral String
  deriving (Eq, Read, Show)

newtype DecimalIntegerLiteralNode = DecimalIntegerLiteral String
  deriving (Eq, Read, Show)

newtype HexIntegerLiteralNode = HexIntegerLiteral String
  deriving (Eq, Read, Show)

newtype IdentifierNode = Identifier String
  deriving (Eq, Read, Show)

newtype HardwareQubitNode = HardwareQubit String
  deriving (Eq, Read, Show)

newtype FloatLiteralNode = FloatLiteral String
  deriving (Eq, Read, Show)

newtype TimingLiteralNode = TimingLiteral String
  deriving (Eq, Read, Show)

newtype BitstringLiteralNode = BitstringLiteral String
  deriving (Eq, Read, Show)

newtype WhitespaceNode = Whitespace String
  deriving (Eq, Read, Show)

newtype NewlineNode = Newline String
  deriving (Eq, Read, Show)

newtype LineCommentNode = LineComment String
  deriving (Eq, Read, Show)

newtype BlockCommentNode = BlockComment String
  deriving (Eq, Read, Show)

newtype VersionSpecifierNode = VersionSpecifier String
  deriving (Eq, Read, Show)

newtype StringLiteralNode = StringLiteral String
  deriving (Eq, Read, Show)

newtype RemainingLineContentNode = RemainingLineContent String
  deriving (Eq, Read, Show)

newtype DefcalPreludeBitshiftOperatorNode = DefcalPreludeBitshiftOperator String
  deriving (Eq, Read, Show)

newtype DefcalPreludeBitstringLiteralNode = DefcalPreludeBitstringLiteral String
  deriving (Eq, Read, Show)

newtype DefcalPreludeBinaryIntegerLiteralNode = DefcalPreludeBinaryIntegerLiteral String
  deriving (Eq, Read, Show)

newtype DefcalPreludeOctalIntegerLiteralNode = DefcalPreludeOctalIntegerLiteral String
  deriving (Eq, Read, Show)

newtype DefcalPreludeDecimalIntegerLiteralNode = DefcalPreludeDecimalIntegerLiteral String
  deriving (Eq, Read, Show)

newtype DefcalPreludeHexIntegerLiteralNode = DefcalPreludeHexIntegerLiteral String
  deriving (Eq, Read, Show)

newtype DefcalPreludeFloatLiteralNode = DefcalPreludeFloatLiteral String
  deriving (Eq, Read, Show)

newtype DefcalPreludeIdentifierNode = DefcalPreludeIdentifier String
  deriving (Eq, Read, Show)

newtype DefcalPreludeHardwareQubitNode = DefcalPreludeHardwareQubit String
  deriving (Eq, Read, Show)

newtype CalibrationBlockNode = CalibrationBlock String
  deriving (Eq, Read, Show)

instance AstNode ProgramNode where
  pretty (Program version statements) = ""

instance AstNode StatementNode where
  pretty (Pragma content) = ""
  pretty (Annotated annotations statement) = ""

instance AstNode AnnotationNode where
  pretty (Annotation annotation content) = ""

instance AstNode StatementContentNode where
  pretty (AliasDeclaration id alias) = ""
  pretty (Assignment indexedId op expr) = ""
  pretty (Barrier gateOperands) = ""
  pretty (Box maybeDesignator) = ""
  pretty Break = "break;"
  pretty (Cal (Just calBlock)) = ""
  pretty (Cal Nothing) = ""
  pretty (CalibrationGrammar strLit) = ""
  pretty (ClassicalDeclaration anyType id (Just declExpr)) = ""
  pretty (ClassicalDeclaration anyType id Nothing) = ""
  pretty (ConstDeclaration scalarType id declExpr) = ""
  pretty Continue = "continue;"
  pretty (Def id argDefs scalarType) = ""
  pretty (Defcal defcalTarget defcalArgs defcalOperands maybeScalarType maybeCalBlock) = ""
  pretty (Delay designator gateOperands) = ""
  pretty End = ""
  pretty (Expression expr) = pretty expr ++ ";"
  pretty (Extern id argDefs scalarType) = ""
  pretty (For scalarType id expr loopStmt) = ""
  pretty (RangeFor scalarType id rangeExpr loopStmt) = ""
  pretty (SetFor scalarType id setExpr loopStmt) = ""
  pretty (Gate id paramIds regIds) = ""
  pretty (GateCall modifiers id exprs maybeDesignator gateOperands) = ""
  pretty (If testExpr thenStmt (Just elseStmt)) =
    "if (" ++ pretty testExpr ++ ") " ++ pretty thenStmt ++ " else " ++ pretty elseStmt
  pretty (If testExpr thenStmt Nothing) = "if (" ++ pretty testExpr ++ ") " ++ pretty thenStmt
  pretty (Include strLit) = ""
  pretty (InputIoDeclaration anyType id) = ""
  pretty (OutputIoDeclaration anyType id) = ""
  pretty (MeasureArrowAssignment gateOperand maybeIndexedId) = ""
  pretty (CregOldStyleDeclaration id maybeDesignator) = ""
  pretty (QregOldStyleDeclaration id maybeDesignator) = ""
  pretty (QuantumDeclaration qubitType id) = ""
  pretty (Reset gateOperand) = ""
  pretty (Return expr) = "return " ++ pretty expr ++ ";"
  pretty (While condExpr loopStmt) = "while (" ++ pretty condExpr ++ ") " ++ pretty loopStmt

instance AstNode ScalarOrArrayTypeNode where
  pretty (Scalar scalarType) = ""
  pretty (Array arrayType) = ""

instance AstNode StatementOrScopeNode where
  pretty (Statement statement) = pretty statement
  pretty (Scope statements) = "{\n" ++ indent (concatMap (\s -> pretty s ++ "\n") statements) ++ "}\n"

instance AstNode AliasExpressionNode where
  pretty (AliasExpression exprs) = ""

instance AstNode ExpressionNode where
  pretty (ParenExpression expr) = ""
  pretty (IndexExpression expr index) = ""
  pretty (UnaryExpression op expr) = ""
  pretty (BinaryExpression exprA op exprB) = ""
  pretty (CastExpression anyType expr) = ""
  pretty (CallExpression id exprs) = ""
  pretty (BinaryIntegerExpression binLit) = ""
  pretty (OctalIntegerExpression octLit) = ""
  pretty (DecimalIntegerExpression decLit) = ""
  pretty (HexIntegerExpression hexLit) = ""
  pretty (FloatExpression floatLit) = ""
  pretty (ImaginaryExpression imagLit) = ""
  pretty (BooleanExpression boolLit) = ""
  pretty (BitstringExpression bitsLit) = ""
  pretty (TimingExpression timingLit) = ""
  pretty (HardwareQubitExpression hwQubit) = ""

instance AstNode GateModifierNode where
  pretty InvGateModifier = ""
  pretty (PowGateModifier expr) = ""
  pretty (CtrlGateModifier (Just expr)) = ""
  pretty (CtrlGateModifier Nothing) = ""
  pretty (NegCtrlGateModifier (Just expr)) = ""
  pretty (NegCtrlGateModifier Nothing) = ""

instance AstNode ScalarTypeNode where
  pretty (BitType maybeDesignator) = ""
  pretty (IntType maybeDesignator) = ""
  pretty (UintType maybeDesignator) = ""
  pretty (FloatType maybeDesignator) = ""
  pretty (AngleType maybeDesignator) = ""
  pretty BoolType = ""
  pretty DurationType = ""
  pretty StretchType = ""
  pretty (ComplexType (Just scalarType)) = ""
  pretty (ComplexType Nothing) = ""

instance AstNode QubitTypeNode where
  pretty (QubitType maybeDesignator) = ""

instance AstNode ArrayTypeNode where
  pretty (ArrayType scalarType exprs) = ""

instance AstNode ArgumentDefinitionNode where
  pretty (ScalarArgument scalarType id) = ""
  pretty (QubitArgument qubitType id) = ""
  pretty (CregArgument id maybeDesignator) = ""
  pretty (QregArgument id maybeDesignator) = ""
  pretty (ArrayArgument arrayRefType id) = ""

instance AstNode ArrayReferenceTypeNode where
  pretty (ReadonlyArrayReferenceType scalarType exprs) = ""
  pretty (MutableArrayReferenceType scalarType exprs) = ""
  pretty (ReadonlyArrayReferenceDimType scalarType expr) = ""
  pretty (MutableArrayReferenceDimType scalarType expr) = ""

instance AstNode DesignatorNode where
  pretty (Designator expr) = ""

instance AstNode DeclarationExpressionNode where
  pretty (ArrayLiteralDeclarationExpression arrayLit) = ""
  pretty (ExpressionDeclarationExpression expr) = ""

instance AstNode DefcalTargetNode where
  pretty MeasureDefcalTarget = ""
  pretty ResetDefcalTarget = ""
  pretty DelayDefcalTarget = ""
  pretty (IdentifierDefcalTarget id) = ""

instance AstNode DefcalArgumentDefinitionNode where
  pretty (ExpressionDefcalArgument expr) = ""
  pretty (ArgumentDefinitionDefcalArgument argDef) = ""

instance AstNode DefcalOperandNode where
  pretty (HardwardDefcal hwQubit) = ""
  pretty (IdentifierDefcal id) = ""

instance AstNode ExternArgumentNode where
  pretty (ScalarExternArgument scalarType) = ""
  pretty (ArrayExternArgument arrayRefType) = ""
  pretty (CregExternArgument maybeDesignator) = ""

instance AstNode GateOperandNode where
  pretty (IdentifierGateOperand indexedId) = ""
  pretty (QubitGateOperand hwQubit) = ""

instance AstNode IndexedIdentifierNode where
  pretty (IndexedIdentifier id indices) = ""

instance AstNode IndexOperatorNode where
  pretty (SetIndex setExpr) = ""
  pretty (IndexList exprs) = ""

instance AstNode RangeOrExpressionIndex where
  pretty (ExpressionIndex expr) = ""
  pretty (RangeIndex rangeExpr) = ""

instance AstNode MeasureExpressionNode where
  pretty (PlainExpression expr) = ""
  pretty (MeasureExpression gateOperand) = ""

instance AstNode RangeExpressionNode where
  pretty (RangeExpression start end maybeStride) = ""

instance AstNode SetExpressionNode where
  pretty (SetExpression exprs) = ""

instance AstNode ArrayLiteralNode where
  pretty (ArrayLiteral elements) = ""

instance AstNode ArrayElementNode where
  pretty (ExpressionArrayElement expr) = ""
  pretty (ArrayArrayElement arrayLit) = ""

instance AstNode AnnotationKeywordNode where
  pretty (AnnotationKeyword str) = str

instance AstNode BooleanLiteralNode where
  pretty (BooleanLiteral str) = str

instance AstNode EqualityOperatorNode where
  pretty (EqualityOperator str) = str

instance AstNode CompoundAssignmentOperatorNode where
  pretty (CompoundAssignmentOperator str) = str

instance AstNode ComparisonOperatorNode where
  pretty (ComparisonOperator str) = str

instance AstNode BitshiftOperatorNode where
  pretty (BitshiftOperator str) = str

instance AstNode ImaginaryLiteralNode where
  pretty (ImaginaryLiteral str) = str

instance AstNode BinaryIntegerLiteralNode where
  pretty (BinaryIntegerLiteral str) = str

instance AstNode OctalIntegerLiteralNode where
  pretty (OctalIntegerLiteral str) = str

instance AstNode DecimalIntegerLiteralNode where
  pretty (DecimalIntegerLiteral str) = str

instance AstNode HexIntegerLiteralNode where
  pretty (HexIntegerLiteral str) = str

instance AstNode IdentifierNode where
  pretty (Identifier str) = str

instance AstNode HardwareQubitNode where
  pretty (HardwareQubit str) = str

instance AstNode FloatLiteralNode where
  pretty (FloatLiteral str) = str

instance AstNode TimingLiteralNode where
  pretty (TimingLiteral str) = str

instance AstNode BitstringLiteralNode where
  pretty (BitstringLiteral str) = str

instance AstNode WhitespaceNode where
  pretty (Whitespace str) = str

instance AstNode NewlineNode where
  pretty (Newline str) = str

instance AstNode LineCommentNode where
  pretty (LineComment str) = str

instance AstNode BlockCommentNode where
  pretty (BlockComment str) = str

instance AstNode VersionSpecifierNode where
  pretty (VersionSpecifier str) = str

instance AstNode StringLiteralNode where
  pretty (StringLiteral str) = str

instance AstNode RemainingLineContentNode where
  pretty (RemainingLineContent str) = str

instance AstNode DefcalPreludeBitshiftOperatorNode where
  pretty (DefcalPreludeBitshiftOperator str) = str

instance AstNode DefcalPreludeBitstringLiteralNode where
  pretty (DefcalPreludeBitstringLiteral str) = str

instance AstNode DefcalPreludeBinaryIntegerLiteralNode where
  pretty (DefcalPreludeBinaryIntegerLiteral str) = str

instance AstNode DefcalPreludeOctalIntegerLiteralNode where
  pretty (DefcalPreludeOctalIntegerLiteral str) = str

instance AstNode DefcalPreludeDecimalIntegerLiteralNode where
  pretty (DefcalPreludeDecimalIntegerLiteral str) = str

instance AstNode DefcalPreludeHexIntegerLiteralNode where
  pretty (DefcalPreludeHexIntegerLiteral str) = str

instance AstNode DefcalPreludeFloatLiteralNode where
  pretty (DefcalPreludeFloatLiteral str) = str

instance AstNode DefcalPreludeIdentifierNode where
  pretty (DefcalPreludeIdentifier str) = str

instance AstNode DefcalPreludeHardwareQubitNode where
  pretty (DefcalPreludeHardwareQubit str) = str

instance AstNode CalibrationBlockNode where
  pretty (CalibrationBlock str) = str

prettyMaybeDesignator :: Maybe DesignatorNode -> String
prettyMaybeDesignator (Just designator) = ""
prettyMaybeDesignator Nothing = ""

indent :: String -> String
indent s = s
