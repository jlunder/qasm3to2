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

data Lexeme = Lexeme SourceRef Token
  deriving (Eq, Read, Show)

data ProgramNode = Program VersionSpecifierNode [StatementNode]
  deriving (Eq, Read, Show)

instance AstNode ProgramNode where
  sourceRef (Program version _) = Nothing

  pretty (Program version statements) = ""

data StatementNode
  = Pragma SourceRef String
  | Annotated SourceRef [AnnotationNode] StatementContentNode
  deriving (Eq, Read, Show)

instance AstNode StatementNode where
  sourceRef (Pragma ref _) = Just ref
  sourceRef (Annotated ref _ _) = Just ref

  pretty (Pragma _ content) = ""
  pretty (Annotated _ annotations statement) = ""

data AnnotationNode = Annotation String String
  deriving (Eq, Read, Show)

instance AstNode AnnotationNode where
  sourceRef (Annotation annotation content) = Nothing
  pretty (Annotation annotation content) = ""

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

  sourceRef (AliasDeclaration id alias) = Nothing
  sourceRef (Assignment indexedId op expr) = Nothing
  sourceRef (Barrier gateOperands) = Nothing
  sourceRef (Box maybeDesignator) = Nothing
  sourceRef Break = Nothing
  sourceRef (Cal (Just calBlock)) = Nothing
  sourceRef (Cal Nothing) = Nothing
  sourceRef (CalibrationGrammar strLit) = Nothing
  sourceRef (ClassicalDeclaration anyType id (Just declExpr)) = Nothing
  sourceRef (ClassicalDeclaration anyType id Nothing) = Nothing
  sourceRef (ConstDeclaration scalarType id declExpr) = Nothing
  sourceRef Continue = Nothing
  sourceRef (Def id argDefs scalarType) = Nothing
  sourceRef (Defcal defcalTarget defcalArgs defcalOperands maybeScalarType maybeCalBlock) = Nothing
  sourceRef (Delay designator gateOperands) = Nothing
  sourceRef End = Nothing
  sourceRef (Expression expr) = Nothing
  sourceRef (Extern id argDefs scalarType) = Nothing
  sourceRef (For scalarType id expr loopStmt) = Nothing
  sourceRef (RangeFor scalarType id rangeExpr loopStmt) = Nothing
  sourceRef (SetFor scalarType id setExpr loopStmt) = Nothing
  sourceRef (Gate id paramIds regIds) = Nothing
  sourceRef (GateCall modifiers id exprs maybeDesignator gateOperands) = Nothing
  sourceRef (If testExpr thenStmt (Just elseStmt)) = Nothing
  sourceRef (If testExpr thenStmt Nothing) = Nothing
  sourceRef (Include strLit) = Nothing
  sourceRef (InputIoDeclaration anyType id) = Nothing
  sourceRef (OutputIoDeclaration anyType id) = Nothing
  sourceRef (MeasureArrowAssignment gateOperand maybeIndexedId) = Nothing
  sourceRef (CregOldStyleDeclaration id maybeDesignator) = Nothing
  sourceRef (QregOldStyleDeclaration id maybeDesignator) = Nothing
  sourceRef (QuantumDeclaration qubitType id) = Nothing
  sourceRef (Reset gateOperand) = Nothing
  sourceRef (Return expr) = Nothing
  sourceRef (While condExpr loopStmt) = Nothing

data ScalarOrArrayTypeNode = Scalar ScalarTypeNode | Array ArrayTypeNode
  deriving (Eq, Read, Show)

instance AstNode ScalarOrArrayTypeNode where
  pretty (Scalar scalarType) = ""
  pretty (Array arrayType) = ""
  sourceRef (Scalar scalarType) = Nothing
  sourceRef (Array arrayType) = Nothing

data StatementOrScopeNode = Statement StatementNode | Scope [StatementNode]
  deriving (Eq, Read, Show)

instance AstNode StatementOrScopeNode where
  pretty (Statement statement) = pretty statement
  pretty (Scope statements) = "{\n" ++ indent (concatMap (\s -> pretty s ++ "\n") statements) ++ "}\n"
  sourceRef (Statement statement) = Nothing
  sourceRef (Scope statements) = Nothing

newtype AliasExpressionNode = AliasExpression [ExpressionNode]
  deriving (Eq, Read, Show)

instance AstNode AliasExpressionNode where
  pretty (AliasExpression exprs) = ""
  sourceRef (AliasExpression exprs) = Nothing

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

  sourceRef (ParenExpression expr) = Nothing
  sourceRef (IndexExpression expr index) = Nothing
  sourceRef (UnaryExpression op expr) = Nothing
  sourceRef (BinaryExpression exprA op exprB) = Nothing
  sourceRef (CastExpression anyType expr) = Nothing
  sourceRef (CallExpression id exprs) = Nothing
  sourceRef (BinaryIntegerExpression binLit) = Nothing
  sourceRef (OctalIntegerExpression octLit) = Nothing
  sourceRef (DecimalIntegerExpression decLit) = Nothing
  sourceRef (HexIntegerExpression hexLit) = Nothing
  sourceRef (FloatExpression floatLit) = Nothing
  sourceRef (ImaginaryExpression imagLit) = Nothing
  sourceRef (BooleanExpression boolLit) = Nothing
  sourceRef (BitstringExpression bitsLit) = Nothing
  sourceRef (TimingExpression timingLit) = Nothing
  sourceRef (HardwareQubitExpression hwQubit) = Nothing

data GateModifierNode
  = InvGateModifier
  | PowGateModifier ExpressionNode
  | CtrlGateModifier (Maybe ExpressionNode)
  | NegCtrlGateModifier (Maybe ExpressionNode)
  deriving (Eq, Read, Show)

instance AstNode GateModifierNode where
  pretty InvGateModifier = ""
  pretty (PowGateModifier expr) = ""
  pretty (CtrlGateModifier (Just expr)) = ""
  pretty (CtrlGateModifier Nothing) = ""
  pretty (NegCtrlGateModifier (Just expr)) = ""
  pretty (NegCtrlGateModifier Nothing) = ""

  sourceRef InvGateModifier = Nothing
  sourceRef (PowGateModifier expr) = Nothing
  sourceRef (CtrlGateModifier (Just expr)) = Nothing
  sourceRef (CtrlGateModifier Nothing) = Nothing
  sourceRef (NegCtrlGateModifier (Just expr)) = Nothing
  sourceRef (NegCtrlGateModifier Nothing) = Nothing

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

  sourceRef (BitType maybeDesignator) = Nothing
  sourceRef (IntType maybeDesignator) = Nothing
  sourceRef (UintType maybeDesignator) = Nothing
  sourceRef (FloatType maybeDesignator) = Nothing
  sourceRef (AngleType maybeDesignator) = Nothing
  sourceRef BoolType = Nothing
  sourceRef DurationType = Nothing
  sourceRef StretchType = Nothing
  sourceRef (ComplexType (Just scalarType)) = Nothing
  sourceRef (ComplexType Nothing) = Nothing

newtype QubitTypeNode = QubitType (Maybe DesignatorNode)
  deriving (Eq, Read, Show)

instance AstNode QubitTypeNode where
  pretty (QubitType maybeDesignator) = ""
  sourceRef (QubitType maybeDesignator) = Nothing

data ArrayTypeNode = ArrayType ScalarTypeNode [ExpressionNode]
  deriving (Eq, Read, Show)

instance AstNode ArrayTypeNode where
  pretty (ArrayType scalarType exprs) = ""
  sourceRef (ArrayType scalarType exprs) = Nothing

data ArgumentDefinitionNode
  = ScalarArgument ScalarTypeNode IdentifierNode
  | QubitArgument QubitTypeNode IdentifierNode
  | CregArgument IdentifierNode (Maybe DesignatorNode)
  | QregArgument IdentifierNode (Maybe DesignatorNode)
  | ArrayArgument ArrayReferenceTypeNode IdentifierNode
  deriving (Eq, Read, Show)

instance AstNode ArgumentDefinitionNode where
  pretty (ScalarArgument scalarType id) = ""
  pretty (QubitArgument qubitType id) = ""
  pretty (CregArgument id maybeDesignator) = ""
  pretty (QregArgument id maybeDesignator) = ""
  pretty (ArrayArgument arrayRefType id) = ""

  sourceRef (ScalarArgument scalarType id) = Nothing
  sourceRef (QubitArgument qubitType id) = Nothing
  sourceRef (CregArgument id maybeDesignator) = Nothing
  sourceRef (QregArgument id maybeDesignator) = Nothing
  sourceRef (ArrayArgument arrayRefType id) = Nothing

data ArrayReferenceTypeNode
  = ReadonlyArrayReferenceType ScalarTypeNode [ExpressionNode]
  | MutableArrayReferenceType ScalarTypeNode [ExpressionNode]
  | ReadonlyArrayReferenceDimType ScalarTypeNode ExpressionNode
  | MutableArrayReferenceDimType ScalarTypeNode ExpressionNode
  deriving (Eq, Read, Show)

instance AstNode ArrayReferenceTypeNode where
  pretty (ReadonlyArrayReferenceType scalarType exprs) = ""
  pretty (MutableArrayReferenceType scalarType exprs) = ""
  pretty (ReadonlyArrayReferenceDimType scalarType expr) = ""
  pretty (MutableArrayReferenceDimType scalarType expr) = ""

  sourceRef (ReadonlyArrayReferenceType scalarType exprs) = Nothing
  sourceRef (MutableArrayReferenceType scalarType exprs) = Nothing
  sourceRef (ReadonlyArrayReferenceDimType scalarType expr) = Nothing
  sourceRef (MutableArrayReferenceDimType scalarType expr) = Nothing

newtype DesignatorNode = Designator ExpressionNode
  deriving (Eq, Read, Show)

instance AstNode DesignatorNode where
  pretty (Designator expr) = ""

data DeclarationExpressionNode
  = ArrayLiteralDeclarationExpression ArrayLiteralNode
  | ExpressionDeclarationExpression MeasureExpressionNode
  deriving (Eq, Read, Show)

instance AstNode DeclarationExpressionNode where
  pretty (ArrayLiteralDeclarationExpression arrayLit) = ""
  pretty (ExpressionDeclarationExpression expr) = ""

data DefcalTargetNode
  = MeasureDefcalTarget
  | ResetDefcalTarget
  | DelayDefcalTarget
  | IdentifierDefcalTarget IdentifierNode
  deriving (Eq, Read, Show)

instance AstNode DefcalTargetNode where
  pretty MeasureDefcalTarget = ""
  pretty ResetDefcalTarget = ""
  pretty DelayDefcalTarget = ""
  pretty (IdentifierDefcalTarget id) = ""

data DefcalArgumentDefinitionNode
  = ExpressionDefcalArgument ExpressionNode
  | ArgumentDefinitionDefcalArgument ArgumentDefinitionNode
  deriving (Eq, Read, Show)

instance AstNode DefcalArgumentDefinitionNode where
  pretty (ExpressionDefcalArgument expr) = ""
  pretty (ArgumentDefinitionDefcalArgument argDef) = ""

data DefcalOperandNode
  = HardwardDefcal HardwareQubitNode
  | IdentifierDefcal IdentifierNode
  deriving (Eq, Read, Show)

instance AstNode DefcalOperandNode where
  pretty (HardwardDefcal hwQubit) = ""
  pretty (IdentifierDefcal id) = ""

data ExternArgumentNode
  = ScalarExternArgument ScalarTypeNode
  | ArrayExternArgument ArrayReferenceTypeNode
  | CregExternArgument (Maybe DesignatorNode)
  deriving (Eq, Read, Show)

instance AstNode ExternArgumentNode where
  pretty (ScalarExternArgument scalarType) = ""
  pretty (ArrayExternArgument arrayRefType) = ""
  pretty (CregExternArgument maybeDesignator) = ""

data GateOperandNode = IdentifierGateOperand IndexedIdentifierNode | QubitGateOperand HardwareQubitNode
  deriving (Eq, Read, Show)

instance AstNode GateOperandNode where
  pretty (IdentifierGateOperand indexedId) = ""
  pretty (QubitGateOperand hwQubit) = ""

data IndexedIdentifierNode = IndexedIdentifier IdentifierNode [IndexOperatorNode]
  deriving (Eq, Read, Show)

instance AstNode IndexedIdentifierNode where
  pretty (IndexedIdentifier id indices) = ""

data IndexOperatorNode = SetIndex SetExpressionNode | IndexList [RangeOrExpressionIndex]
  deriving (Eq, Read, Show)

instance AstNode IndexOperatorNode where
  pretty (SetIndex setExpr) = ""
  pretty (IndexList exprs) = ""

data RangeOrExpressionIndex = ExpressionIndex ExpressionNode | RangeIndex RangeExpressionNode
  deriving (Eq, Read, Show)

instance AstNode RangeOrExpressionIndex where
  pretty (ExpressionIndex expr) = ""
  pretty (RangeIndex rangeExpr) = ""

data MeasureExpressionNode
  = PlainExpression ExpressionNode
  | MeasureExpression GateOperandNode
  deriving (Eq, Read, Show)

instance AstNode MeasureExpressionNode where
  pretty (PlainExpression expr) = ""
  pretty (MeasureExpression gateOperand) = ""

data RangeExpressionNode = RangeExpression ExpressionNode ExpressionNode (Maybe ExpressionNode)
  deriving (Eq, Read, Show)

instance AstNode RangeExpressionNode where
  pretty (RangeExpression start end maybeStride) = ""

newtype SetExpressionNode = SetExpression [ExpressionNode]
  deriving (Eq, Read, Show)

instance AstNode SetExpressionNode where
  pretty (SetExpression exprs) = ""

newtype ArrayLiteralNode = ArrayLiteral [ArrayElementNode]
  deriving (Eq, Read, Show)

instance AstNode ArrayLiteralNode where
  pretty (ArrayLiteral elements) = ""

data ArrayElementNode = ExpressionArrayElement ExpressionNode | ArrayArrayElement ArrayLiteralNode
  deriving (Eq, Read, Show)

instance AstNode ArrayElementNode where
  pretty (ExpressionArrayElement expr) = ""
  pretty (ArrayArrayElement arrayLit) = ""

newtype BooleanLiteralNode = BooleanLiteral String
  deriving (Eq, Read, Show)

instance AstNode BooleanLiteralNode where
  pretty (BooleanLiteral str) = str
  sourceRef (BooleanLiteral str) = Nothing

newtype EqualityOperatorNode = EqualityOperator String
  deriving (Eq, Read, Show)

instance AstNode EqualityOperatorNode where
  pretty (EqualityOperator str) = str
  sourceRef (EqualityOperator str) = Nothing

newtype CompoundAssignmentOperatorNode = CompoundAssignmentOperator String
  deriving (Eq, Read, Show)

instance AstNode CompoundAssignmentOperatorNode where
  pretty (CompoundAssignmentOperator str) = str
  sourceRef (CompoundAssignmentOperator str) = Nothing

newtype ComparisonOperatorNode = ComparisonOperator String
  deriving (Eq, Read, Show)

instance AstNode ComparisonOperatorNode where
  pretty (ComparisonOperator str) = str
  sourceRef (ComparisonOperator str) = Nothing

newtype BitshiftOperatorNode = BitshiftOperator String
  deriving (Eq, Read, Show)

instance AstNode BitshiftOperatorNode where
  pretty (BitshiftOperator str) = str
  sourceRef (BitshiftOperator str) = Nothing

newtype ImaginaryLiteralNode = ImaginaryLiteral String
  deriving (Eq, Read, Show)

instance AstNode ImaginaryLiteralNode where
  pretty (ImaginaryLiteral str) = str
  sourceRef (ImaginaryLiteral str) = Nothing

newtype BinaryIntegerLiteralNode = BinaryIntegerLiteral String
  deriving (Eq, Read, Show)

instance AstNode BinaryIntegerLiteralNode where
  pretty (BinaryIntegerLiteral str) = str
  sourceRef (BinaryIntegerLiteral str) = Nothing

newtype OctalIntegerLiteralNode = OctalIntegerLiteral String
  deriving (Eq, Read, Show)

instance AstNode OctalIntegerLiteralNode where
  pretty (OctalIntegerLiteral str) = str
  sourceRef (OctalIntegerLiteral str) = Nothing

newtype DecimalIntegerLiteralNode = DecimalIntegerLiteral String
  deriving (Eq, Read, Show)

instance AstNode DecimalIntegerLiteralNode where
  pretty (DecimalIntegerLiteral str) = str
  sourceRef (DecimalIntegerLiteral str) = Nothing

newtype HexIntegerLiteralNode = HexIntegerLiteral String
  deriving (Eq, Read, Show)

instance AstNode HexIntegerLiteralNode where
  pretty (HexIntegerLiteral str) = str
  sourceRef (HexIntegerLiteral str) = Nothing

newtype IdentifierNode = Identifier String
  deriving (Eq, Read, Show)

instance AstNode IdentifierNode where
  pretty (Identifier str) = str
  sourceRef (Identifier str) = Nothing

newtype HardwareQubitNode = HardwareQubit String
  deriving (Eq, Read, Show)

instance AstNode HardwareQubitNode where
  pretty (HardwareQubit str) = str
  sourceRef (HardwareQubit str) = Nothing

newtype FloatLiteralNode = FloatLiteral String
  deriving (Eq, Read, Show)

instance AstNode FloatLiteralNode where
  pretty (FloatLiteral str) = str
  sourceRef (FloatLiteral str) = Nothing

newtype TimingLiteralNode = TimingLiteral String
  deriving (Eq, Read, Show)

instance AstNode TimingLiteralNode where
  pretty (TimingLiteral str) = str
  sourceRef (TimingLiteral str) = Nothing

newtype BitstringLiteralNode = BitstringLiteral String
  deriving (Eq, Read, Show)

instance AstNode BitstringLiteralNode where
  pretty (BitstringLiteral str) = str
  sourceRef (BitstringLiteral str) = Nothing

newtype WhitespaceNode = Whitespace String
  deriving (Eq, Read, Show)

instance AstNode WhitespaceNode where
  pretty (Whitespace str) = str
  sourceRef (Whitespace str) = Nothing

newtype NewlineNode = Newline String
  deriving (Eq, Read, Show)

instance AstNode NewlineNode where
  pretty (Newline str) = str
  sourceRef (Newline str) = Nothing

newtype LineCommentNode = LineComment String
  deriving (Eq, Read, Show)

instance AstNode LineCommentNode where
  pretty (LineComment str) = str
  sourceRef (LineComment str) = Nothing

newtype BlockCommentNode = BlockComment String
  deriving (Eq, Read, Show)

instance AstNode BlockCommentNode where
  pretty (BlockComment str) = str
  sourceRef (BlockComment str) = Nothing

newtype VersionSpecifierNode = VersionSpecifier Lexeme
  deriving (Eq, Read, Show)

instance AstNode VersionSpecifierNode where
  pretty (VersionSpecifier (Lexeme _ (VersionSpecifierToken str))) = str
  sourceRef (VersionSpecifier (Lexeme ref _)) = Just ref

newtype StringLiteralNode = StringLiteral String
  deriving (Eq, Read, Show)

instance AstNode StringLiteralNode where
  pretty (StringLiteral str) = str
  sourceRef (StringLiteral str) = Nothing

newtype DefcalPreludeBitshiftOperatorNode = DefcalPreludeBitshiftOperator String
  deriving (Eq, Read, Show)

instance AstNode DefcalPreludeBitshiftOperatorNode where
  pretty (DefcalPreludeBitshiftOperator str) = str
  sourceRef (DefcalPreludeBitshiftOperator str) = Nothing

newtype DefcalPreludeBitstringLiteralNode = DefcalPreludeBitstringLiteral String
  deriving (Eq, Read, Show)

instance AstNode DefcalPreludeBitstringLiteralNode where
  pretty (DefcalPreludeBitstringLiteral str) = str
  sourceRef (DefcalPreludeBitstringLiteral str) = Nothing

newtype DefcalPreludeBinaryIntegerLiteralNode = DefcalPreludeBinaryIntegerLiteral String
  deriving (Eq, Read, Show)

instance AstNode DefcalPreludeBinaryIntegerLiteralNode where
  pretty (DefcalPreludeBinaryIntegerLiteral str) = str
  sourceRef (DefcalPreludeBinaryIntegerLiteral str) = Nothing

newtype DefcalPreludeOctalIntegerLiteralNode = DefcalPreludeOctalIntegerLiteral String
  deriving (Eq, Read, Show)

instance AstNode DefcalPreludeOctalIntegerLiteralNode where
  pretty (DefcalPreludeOctalIntegerLiteral str) = str
  sourceRef (DefcalPreludeOctalIntegerLiteral str) = Nothing

newtype DefcalPreludeDecimalIntegerLiteralNode = DefcalPreludeDecimalIntegerLiteral String
  deriving (Eq, Read, Show)

instance AstNode DefcalPreludeDecimalIntegerLiteralNode where
  pretty (DefcalPreludeDecimalIntegerLiteral str) = str
  sourceRef (DefcalPreludeDecimalIntegerLiteral str) = Nothing

newtype DefcalPreludeHexIntegerLiteralNode = DefcalPreludeHexIntegerLiteral String
  deriving (Eq, Read, Show)

instance AstNode DefcalPreludeHexIntegerLiteralNode where
  pretty (DefcalPreludeHexIntegerLiteral str) = str
  sourceRef (DefcalPreludeHexIntegerLiteral str) = Nothing

newtype DefcalPreludeFloatLiteralNode = DefcalPreludeFloatLiteral String
  deriving (Eq, Read, Show)

instance AstNode DefcalPreludeFloatLiteralNode where
  pretty (DefcalPreludeFloatLiteral str) = str
  sourceRef (DefcalPreludeFloatLiteral str) = Nothing

newtype DefcalPreludeIdentifierNode = DefcalPreludeIdentifier String
  deriving (Eq, Read, Show)

instance AstNode DefcalPreludeIdentifierNode where
  pretty (DefcalPreludeIdentifier str) = str
  sourceRef (DefcalPreludeIdentifier str) = Nothing

newtype DefcalPreludeHardwareQubitNode = DefcalPreludeHardwareQubit String
  deriving (Eq, Read, Show)

instance AstNode DefcalPreludeHardwareQubitNode where
  pretty (DefcalPreludeHardwareQubit str) = str
  sourceRef (DefcalPreludeHardwareQubit str) = Nothing

newtype CalibrationBlockNode = CalibrationBlock String
  deriving (Eq, Read, Show)

instance AstNode CalibrationBlockNode where
  pretty (CalibrationBlock str) = str
  sourceRef (CalibrationBlock str) = Nothing

prettyMaybeDesignator :: Maybe DesignatorNode -> String
prettyMaybeDesignator (Just designator) = ""
prettyMaybeDesignator Nothing = ""

indent :: String -> String
indent block = concatMap (\s -> "  " ++ s ++ "\n") $ lines block
