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
  | AnnotationKeyword String
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
  | BooleanLiteral String
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
  | EqualityOperator String
  | CompoundAssignmentOperator String
  | ComparisonOperator String
  | BitshiftOperator String
  | ImagToken
  | ImaginaryLiteral String
  | BinaryintegerLiteral String
  | OctalintegerLiteral String
  | DecimalintegerLiteral String
  | HexintegerLiteral String
  | Identifier String
  | HardwareQubit String
  | FloatLiteral String
  | TimingLiteral String
  | BitstringLiteral String
  | Whitespace String
  | Newline String
  | LineComment String
  | BlockComment String
  | VersionIdentiferWhitespaceToken
  | VersionSpecifier String
  | ArbitraryStringWhitespaceToken
  | StringLiteral String
  | EatInitialSpaceToken
  | EatLineEndToken
  | RemainingLineContent String
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
  | DefcalPreludeBitshiftOperator String
  | DefcalPreludeBitstringLiteral String
  | DefcalPreludeBinaryIntegerLiteral String
  | DefcalPreludeOctalIntegerLiteral String
  | DefcalPreludeDecimalIntegerLiteral String
  | DefcalPreludeHexIntegerLiteral String
  | DefcalPreludeFloatLiteral String
  | DefcalPreludeMeasureToken
  | DefcalPreludeDelayToken
  | DefcalPreludeResetToken
  | DefcalPreludeIdentifier String
  | DefcalPreludeHardwareQubit String
  | CalibrationBlock String
  | CalBlockRbraceToken
  deriving (Eq, Ord, Read, Show)

data ProgramNode = Program VersionNode [StatementNode]

newtype VersionNode = Version Lexeme

data StatementNode
  = Pragma Lexeme
  | Annotated [AnnotationNode] StatementContentNode

data AnnotationNode = Annotation Lexeme Lexeme

data StatementContentNode
  = AliasDeclaration Lexeme AliasExpressionNode
  | Assignment IndexedIdentifierNode Lexeme ExpressionNode
  | AssignmentMeasure IndexedIdentifierNode Lexeme MeasureExpressionNode
  | Barrier [GateOperandNode]
  | Box (Maybe DesignatorNode)
  | Break
  | Cal (Maybe CalibrationBlockNode)
  | CalibrationGrammar Lexeme
  | ClassicalDeclaration ScalarOrArrayTypeNode Lexeme (Maybe DeclarationExpressionNode)
  | ConstDeclaration ScalarTypeNode Lexeme DeclarationExpressionNode
  | Continue
  | Def Lexeme [ArgumentDefinitionNode] ReturnSignatureNode
  | Defcal Lexeme [DefcalArgumentDefinitionNode] [Lexeme] (Maybe ReturnSignatureNode) (Maybe CalibrationBlockNode)
  | Delay DesignatorNode [GateOperandNode]
  | End
  | Expression ExpressionNode
  | Extern Lexeme [ArgumentDefinitionNode] ReturnSignatureNode
  | For ScalarTypeNode Lexeme ExpressionNode StatementOrScopeNode
  | ForRange ScalarTypeNode Lexeme RangeExpressionNode StatementOrScopeNode
  | ForSet ScalarTypeNode Lexeme SetExpressionNode StatementOrScopeNode
  | Gate Lexeme [Lexeme] [Lexeme]
  | GateCall [GateModifierNode] Lexeme [ExpressionNode] (Maybe DesignatorNode) [GateOperandNode]
  | If ExpressionNode StatementOrScopeNode (Maybe StatementOrScopeNode)
  | Include Lexeme
  | IoDeclaration Lexeme ScalarOrArrayTypeNode Lexeme
  | MeasureArrowAssignment MeasureExpressionNode (Maybe IndexedIdentifierNode)
  | OldStyleDeclaration Lexeme Lexeme (Maybe DesignatorNode)
  | QuantumDeclaration QubitTypeNode Lexeme
  | Reset GateOperandNode
  | Return ExpressionNode
  | ReturnMeasure MeasureExpressionNode
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
  | CallExpression Lexeme [ExpressionNode]
  | LiteralExpression Lexeme

newtype ReturnSignatureNode = ReturnSignature ScalarTypeNode

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
  = ScalarArgument ScalarTypeNode Lexeme
  | QubitArgument QubitTypeNode Lexeme
  | CregArgument Lexeme (Maybe DesignatorNode)
  | QregArgument Lexeme (Maybe DesignatorNode)
  | ArrayArgument ArrayReferenceTypeNode Lexeme

data ArrayReferenceTypeNode
  = ArrayReferenceType Lexeme ScalarTypeNode [ExpressionNode]
  | ArrayReferenceDimType Lexeme ScalarTypeNode ExpressionNode

newtype DesignatorNode = Designator ExpressionNode

data CalibrationBlockNode = Fooooooo

data DeclarationExpressionNode
  = ArrayLiteralDeclarationExpression ArrayLiteralNode
  | ExpressionDeclarationExpression ExpressionNode
  | MeasureExpressionDeclarationExpression MeasureExpressionNode

data DefcalArgumentDefinitionNode
  = ExpressionDefcalArgument ExpressionNode
  | ArgumentDefinitionDefcalArgument ArgumentDefinitionNode

data ExternArgumentNode
  = ScalarExternArgument ScalarTypeNode
  | ArrayExternArgument ArrayReferenceTypeNode
  | CregExternArgument (Maybe DesignatorNode)

data GateOperandNode = IdentifierGateOperand IndexedIdentifierNode | QubitGateOperand Lexeme

data IndexedIdentifierNode = IndexedIdentifier Lexeme [IndexOperatorNode]

data IndexOperatorNode = SetIndex SetExpressionNode | IndexList [RangeOrExpressionIndex]

data RangeOrExpressionIndex = ExpressionIndex ExpressionNode | RangeIndex RangeExpressionNode

newtype MeasureExpressionNode = MeasureExpression GateOperandNode

data RangeExpressionNode = RangeExpression (Maybe ExpressionNode) (Maybe ExpressionNode) (Maybe ExpressionNode)

newtype SetExpressionNode = SetExpression [ExpressionNode]

newtype ArrayLiteralNode = ArrayLiteral [ArrayElementNode]

data ArrayElementNode = ExpressionArrayElement ExpressionNode | ArrayArrayElement ArrayLiteralNode

