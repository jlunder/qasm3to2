{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Qasm3 where

data Lexeme
  = OpenqasmToken
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
  | AnnotationKeywordLexeme String
  | InputToken
  | OutputToken
  | ConstToken
  | ReadOnlyToken
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
  | BooleanLiteralLexeme String
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
  | EqualityOperatorLexeme String
  | CompoundAssignmentOperatorLexeme String
  | ComparisonOperatorLexeme String
  | BitshiftOperatorLexeme String
  | ImagToken
  | ImaginaryLiteralLexeme String
  | BinaryintegerLiteralLexeme String
  | OctalintegerLiteralLexeme String
  | DecimalintegerLiteralLexeme String
  | HexintegerLiteralLexeme String
  | IdentifierLexeme String
  | HardwareQubitLexeme String
  | FloatLiteralLexeme String
  | TimingLiteralLexeme String
  | BitstringLiteralLexeme String
  | WhitespaceLexeme String
  | NewlineLexeme String
  | LineCommentLexeme String
  | BlockCommentLexeme String
  | VersionIdentiferWhitespaceToken
  | VersionSpecifierLexeme String
  | ArbitraryStringWhitespaceToken
  | StringLiteralLexeme String
  | EatInitialSpaceToken
  | EatLineEndToken
  | RemainingLineContentLexeme String
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
  | DefcalPreludeBitshiftOperatorLexeme String
  | DefcalPreludeBitstringLiteralLexeme String
  | DefcalPreludeBinaryIntegerLiteralLexeme String
  | DefcalPreludeOctalIntegerLiteralLexeme String
  | DefcalPreludeDecimalIntegerLiteralLexeme String
  | DefcalPreludeHexIntegerLiteralLexeme String
  | DefcalPreludeFloatLiteralLexeme String
  | DefcalPreludeMeasureToken
  | DefcalPreludeDelayToken
  | DefcalPreludeResetToken
  | DefcalPreludeIdentifierLexeme String
  | DefcalPreludeHardwareQubitLexeme String
  | CalibrationBlockLexeme String
  | CalBlockRbraceToken
  deriving (Eq, Ord, Read, Show)

data ProgramNode = Program VersionSpecifierNode [StatementNode]

data StatementNode
  = Pragma RemainingLineContentNode
  | Annotated [AnnotationNode] StatementContentNode

data AnnotationNode = Annotation AnnotationKeywordNode RemainingLineContentNode

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

data ScalarOrArrayTypeNode = Scalar ScalarTypeNode | Array ArrayTypeNode

data StatementOrScopeNode = Statement StatementNode | Scope [StatementNode]

newtype AliasExpressionNode = AliasExpression [ExpressionNode]

data ExpressionNode
  = ParenExpression ExpressionNode
  | IndexExpression ExpressionNode IndexOperatorNode
  | UnaryExpression Lexeme ExpressionNode
  | BinaryExpression ExpressionNode Lexeme ExpressionNode
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

data GateModifierNode
  = InvGateModifier
  | PowGateModifier ExpressionNode
  | CtrlGateModifier (Maybe ExpressionNode)
  | NegCtrlGateModifier (Maybe ExpressionNode)

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

newtype QubitTypeNode = QubitType (Maybe DesignatorNode)

data ArrayTypeNode = ArrayType ScalarTypeNode [ExpressionNode]

data ArgumentDefinitionNode
  = ScalarArgument ScalarTypeNode IdentifierNode
  | QubitArgument QubitTypeNode IdentifierNode
  | CregArgument IdentifierNode (Maybe DesignatorNode)
  | QregArgument IdentifierNode (Maybe DesignatorNode)
  | ArrayArgument ArrayReferenceTypeNode IdentifierNode

data ArrayReferenceTypeNode
  = ReadonlyArrayReferenceType ScalarTypeNode [ExpressionNode]
  | MutableArrayReferenceType ScalarTypeNode [ExpressionNode]
  | ReadonlyArrayReferenceDimType ScalarTypeNode ExpressionNode
  | MutableArrayReferenceDimType ScalarTypeNode ExpressionNode

newtype DesignatorNode = Designator ExpressionNode

data DeclarationExpressionNode
  = ArrayLiteralDeclarationExpression ArrayLiteralNode
  | ExpressionDeclarationExpression MeasureExpressionNode

data DefcalTargetNode
  = MeasureDefcalTarget
  | ResetDefcalTarget
  | DelayDefcalTarget
  | IdentifierDefcalTarget IdentifierNode;

data DefcalArgumentDefinitionNode
  = ExpressionDefcalArgument ExpressionNode
  | ArgumentDefinitionDefcalArgument ArgumentDefinitionNode

data DefcalOperandNode
  = HardwardDefcal HardwareQubitNode
  | IdentifierDefcal IdentifierNode

data ExternArgumentNode
  = ScalarExternArgument ScalarTypeNode
  | ArrayExternArgument ArrayReferenceTypeNode
  | CregExternArgument (Maybe DesignatorNode)

data GateOperandNode = IdentifierGateOperand IndexedIdentifierNode | QubitGateOperand HardwareQubitNode

data IndexedIdentifierNode = IndexedIdentifier IdentifierNode [IndexOperatorNode]

data IndexOperatorNode = SetIndex SetExpressionNode | IndexList [RangeOrExpressionIndex]

data RangeOrExpressionIndex = ExpressionIndex ExpressionNode | RangeIndex RangeExpressionNode

data MeasureExpressionNode
  = PlainExpression ExpressionNode
  | MeasureExpression GateOperandNode

data RangeExpressionNode = RangeExpression (Maybe ExpressionNode) (Maybe ExpressionNode) (Maybe ExpressionNode)

newtype SetExpressionNode = SetExpression [ExpressionNode]

newtype ArrayLiteralNode = ArrayLiteral [ArrayElementNode]

data ArrayElementNode = ExpressionArrayElement ExpressionNode | ArrayArrayElement ArrayLiteralNode

newtype AnnotationKeywordNode = AnnotationKeyword String

newtype BooleanLiteralNode = BooleanLiteral String

newtype EqualityOperatorNode = EqualityOperator String

newtype CompoundAssignmentOperatorNode = CompoundAssignmentOperator String

newtype ComparisonOperatorNode = ComparisonOperator String

newtype BitshiftOperatorNode = BitshiftOperator String

newtype ImaginaryLiteralNode = ImaginaryLiteral String

newtype BinaryIntegerLiteralNode = BinaryIntegerLiteral String

newtype OctalIntegerLiteralNode = OctalIntegerLiteral String

newtype DecimalIntegerLiteralNode = DecimalIntegerLiteral String

newtype HexIntegerLiteralNode = HexIntegerLiteral String

newtype IdentifierNode = Identifier String

newtype HardwareQubitNode = HardwareQubit String

newtype FloatLiteralNode = FloatLiteral String

newtype TimingLiteralNode = TimingLiteral String

newtype BitstringLiteralNode = BitstringLiteral String

newtype WhitespaceNode = Whitespace String

newtype NewlineNode = Newline String

newtype LineCommentNode = LineComment String

newtype BlockCommentNode = BlockComment String

newtype VersionSpecifierNode = VersionSpecifier String

newtype StringLiteralNode = StringLiteral String

newtype RemainingLineContentNode = RemainingLineContent String

newtype DefcalPreludeBitshiftOperatorNode = DefcalPreludeBitshiftOperator String

newtype DefcalPreludeBitstringLiteralNode = DefcalPreludeBitstringLiteral String

newtype DefcalPreludeBinaryIntegerLiteralNode = DefcalPreludeBinaryIntegerLiteral String

newtype DefcalPreludeOctalIntegerLiteralNode = DefcalPreludeOctalIntegerLiteral String

newtype DefcalPreludeDecimalIntegerLiteralNode = DefcalPreludeDecimalIntegerLiteral String

newtype DefcalPreludeHexIntegerLiteralNode = DefcalPreludeHexIntegerLiteral String

newtype DefcalPreludeFloatLiteralNode = DefcalPreludeFloatLiteral String

newtype DefcalPreludeIdentifierNode = DefcalPreludeIdentifier String

newtype DefcalPreludeHardwareQubitNode = DefcalPreludeHardwareQubit String

newtype CalibrationBlockNode = CalibrationBlock String
