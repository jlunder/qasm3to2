{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use fmap" #-}
module Qasm3Arbitrary where

import Control.Monad
import Qasm3
import Test.QuickCheck

-- Normalization removes the SourceRef from a Normalizable, so that ASTs can be
-- compared irrespective of how the tree was constructed from source text
class Normalizable a where
  norm :: a -> a
  norm = id

instance Normalizable Lexeme where
  norm (Lexeme a t) = Lexeme Nothing t

instance Normalizable ProgramNode where
  norm (Program lex ver prog) = Program (norm lex) (norm ver) (map norm prog)

instance Normalizable StatementNode where
  norm (Pragma lex line) = Pragma (norm lex) (norm line)
  norm (Annotated annots stmt) = Annotated (map norm annots) (norm stmt)

instance Normalizable AnnotationNode where
  norm (Annotation lex line) = Annotation (norm lex) (norm line)

instance Normalizable StatementOrScopeNode where
  norm (Statement stmt) = Statement (norm stmt)
  norm (Scope stmts) = Scope (map norm stmts)

instance Normalizable StatementContentNode where
  norm (AliasDeclaration lex ident aliasExpr) = AliasDeclaration (norm lex) (norm ident) (norm aliasExpr)
  norm (Assignment ident assn expr) = Assignment (norm ident) (norm assn) (norm expr)
  norm (Barrier lex gateOps) = Barrier (norm lex) (map norm gateOps)
  norm (Box lex maybeDsgn stmts) = Box (norm lex) ((Just . norm) =<< maybeDsgn) (map norm stmts)
  norm (Break lex) = Break (norm lex)
  norm (Cal lex calBlock) = Cal (norm lex) (norm calBlock)
  norm (CalibrationGrammar lex calGmr) = CalibrationGrammar (norm lex) (norm calGmr)
  norm (ClassicalDeclaration declType ident maybeDeclExpr) =
    ClassicalDeclaration (norm declType) (norm ident) ((Just . norm) =<< maybeDeclExpr)
  norm (ConstDeclaration lex constType ident declExpr) =
    ConstDeclaration (norm lex) (norm constType) (norm ident) (norm declExpr)
  norm (Continue lex) = Continue (norm lex)
  norm (Def lex ident argDefs maybeRtnType stmts) =
    Def (norm lex) (norm ident) (map norm argDefs) ((Just . norm) =<< maybeRtnType) (map norm stmts)
  norm (Defcal lex tgt argDefs ops maybeRtnType defcalBlock) =
    Defcal (norm lex) (norm tgt) (map norm argDefs) (map norm ops) ((Just . norm) =<< maybeRtnType) (norm defcalBlock)
  norm (Delay lex timeArg gateOps) = Delay (norm lex) (norm timeArg) (map norm gateOps)
  norm (End lex) = End (norm lex)
  norm (Expression expr) = Expression (norm expr)
  norm (Extern lex ident args maybeRtnType) =
    Extern (norm lex) (norm ident) (map norm args) ((Just . norm) =<< maybeRtnType)
  norm (For lex itType itIdent itRange body) = For (norm lex) (norm itType) (norm itIdent) (norm itRange) (norm body)
  norm (RangeFor lex itType itIdent itRange body) =
    RangeFor (norm lex) (norm itType) (norm itIdent) (norm itRange) (norm body)
  norm (SetFor lex itType itIdent itRange body) =
    SetFor (norm lex) (norm itType) (norm itIdent) (norm itRange) (norm body)
  norm (Gate lex ident args qArgs stmts) = Gate (norm lex) (norm ident) (map norm args) (map norm qArgs) (map norm stmts)
  norm (GateCall gateMods ident args maybeDsgn gateOps) =
    GateCall (map norm gateMods) (norm ident) (map norm args) ((Just . norm) =<< maybeDsgn) (map norm gateOps)
  norm (If lex cond ifBody maybeElseBody) = If (norm lex) (norm cond) (norm ifBody) ((Just . norm) =<< maybeElseBody)
  norm (Include lex inclFile) = Include (norm lex) (norm inclFile)
  norm (InputIoDeclaration lex declType ident) = InputIoDeclaration (norm lex) (norm declType) (norm ident)
  norm (OutputIoDeclaration lex declType ident) = OutputIoDeclaration (norm lex) (norm declType) (norm ident)
  norm (MeasureArrowAssignment lex gateOp maybeTgt) =
    MeasureArrowAssignment (norm lex) (norm gateOp) ((Just . norm) =<< maybeTgt)
  norm (CregOldStyleDeclaration lex ident maybeDsgn) =
    CregOldStyleDeclaration (norm lex) (norm ident) ((Just . norm) =<< maybeDsgn)
  norm (QregOldStyleDeclaration lex ident maybeDsgn) =
    QregOldStyleDeclaration (norm lex) (norm ident) ((Just . norm) =<< maybeDsgn)
  norm (QuantumDeclaration qbType ident) = QuantumDeclaration (norm qbType) (norm ident)
  norm (Reset lex gateOp) = Reset (norm lex) (norm gateOp)
  norm (Return lex maybeExpr) = Return (norm lex) ((Just . norm) =<< maybeExpr)
  norm (While lex cond body) = While (norm lex) (norm cond) (norm body)

instance Normalizable ScalarOrArrayTypeNode where
  norm (Scalar sclType) = Scalar (norm sclType)
  norm (Array aryType) = Array (norm aryType)

instance Normalizable ExpressionNode where
  norm (ParenExpression expr) = ParenExpression (norm expr)
  norm (IndexExpression expr index) = IndexExpression (norm expr) (norm index)
  norm (UnaryOperatorExpression op expr) = UnaryOperatorExpression (norm op) (norm expr)
  norm (BinaryOperatorExpression exprA op exprB) = BinaryOperatorExpression (norm exprA) (norm op) (norm exprB)
  norm (CastExpression castType expr) = CastExpression (norm castType) (norm expr)
  norm (DurationOfExpression lex stmts) = DurationOfExpression (norm lex) (map norm stmts)
  norm (CallExpression ident argExprs) = CallExpression (norm ident) (map norm argExprs)
  norm (Identifier ident) = Identifier (norm ident)
  norm (BinaryIntegerLiteral lit) = BinaryIntegerLiteral (norm lit)
  norm (OctalIntegerLiteral lit) = OctalIntegerLiteral (norm lit)
  norm (DecimalIntegerLiteral lit) = DecimalIntegerLiteral (norm lit)
  norm (HexIntegerLiteral lit) = HexIntegerLiteral (norm lit)
  norm (FloatLiteral lit) = FloatLiteral (norm lit)
  norm (ImaginaryLiteral lit) = ImaginaryLiteral (norm lit)
  norm (BooleanLiteral lit) = BooleanLiteral (norm lit)
  norm (BitstringLiteral lit) = BitstringLiteral (norm lit)
  norm (TimingLiteral lit) = TimingLiteral (norm lit)
  norm (HardwareQubitLiteral lit) = HardwareQubitLiteral (norm lit)

instance Normalizable AliasExpressionNode where
  norm (AliasExpression exprs) = AliasExpression (map norm exprs)

instance Normalizable DeclarationExpressionNode where
  norm (ArrayLiteralDeclarationExpression aryLit) = ArrayLiteralDeclarationExpression (norm aryLit)
  norm (ExpressionDeclarationExpression expr) = ExpressionDeclarationExpression (norm expr)

instance Normalizable MeasureExpressionNode where
  norm (PlainExpression expr) = PlainExpression (norm expr)
  norm (MeasureExpression lex gateOp) = MeasureExpression (norm lex) (norm gateOp)

instance Normalizable RangeOrExpressionIndexNode where
  norm (ExpressionIndex expr) = ExpressionIndex (norm expr)
  norm (RangeIndex expr) = RangeIndex (norm expr)

instance Normalizable RangeExpressionNode where
  norm (RangeExpression _ maybeStart maybeEnd maybeStep) =
    RangeExpression Nothing ((Just . norm) =<< maybeStart) ((Just . norm) =<< maybeEnd) ((Just . norm) =<< maybeStep)

instance Normalizable SetExpressionNode where
  norm (SetExpression exprs) = SetExpression (map norm exprs)

instance Normalizable ArrayLiteralNode where
  norm (ArrayLiteral arrayLits) = ArrayLiteral (map norm arrayLits)

instance Normalizable ArrayLiteralElementNode where
  norm (ExpressionArrayElement expr) = ExpressionArrayElement (norm expr)
  norm (ArrayArrayElement aryLit) = ArrayArrayElement (norm aryLit)

instance Normalizable IndexOperatorNode where
  norm (SetIndex setExpr) = SetIndex (norm setExpr)
  norm (IndexList indices) = IndexList (map norm indices)

instance Normalizable IndexedIdentifierNode where
  norm (IndexedIdentifier lex indices) = IndexedIdentifier (norm lex) (map norm indices)

instance Normalizable GateModifierNode where
  norm (InvGateModifier lex) = InvGateModifier (norm lex)
  norm (PowGateModifier lex expr) = PowGateModifier (norm lex) (norm expr)
  norm (CtrlGateModifier lex maybeExpr) = CtrlGateModifier (norm lex) ((Just . norm) =<< maybeExpr)
  norm (NegCtrlGateModifier lex maybeExpr) = NegCtrlGateModifier (norm lex) ((Just . norm) =<< maybeExpr)

instance Normalizable ScalarTypeNode where
  norm (BitType lex maybeDsgn) = BitType (norm lex) ((Just . norm) =<< maybeDsgn)
  norm (IntType lex maybeDsgn) = IntType (norm lex) ((Just . norm) =<< maybeDsgn)
  norm (UintType lex maybeDsgn) = UintType (norm lex) ((Just . norm) =<< maybeDsgn)
  norm (FloatType lex maybeDsgn) = FloatType (norm lex) ((Just . norm) =<< maybeDsgn)
  norm (AngleType lex maybeDsgn) = AngleType (norm lex) ((Just . norm) =<< maybeDsgn)
  norm (BoolType lex) = BoolType (norm lex)
  norm (DurationType lex) = DurationType (norm lex)
  norm (StretchType lex) = StretchType (norm lex)
  norm (ComplexType lex maybeType) = ComplexType (norm lex) ((Just . norm) =<< maybeType)

instance Normalizable QubitTypeNode where
  norm (QubitType lex maybeDsgn) = QubitType (norm lex) ((Just . norm) =<< maybeDsgn)

instance Normalizable ArrayTypeNode where
  norm (ArrayType lex elemType dimExprs) = ArrayType (norm lex) (norm elemType) (map norm dimExprs)

instance Normalizable ArrayReferenceTypeNode where
  norm (ReadonlyArrayReferenceType lex elemType sizeExprs) =
    ReadonlyArrayReferenceType (norm lex) (norm elemType) (map norm sizeExprs)
  norm (MutableArrayReferenceType lex elemType sizeExprs) =
    MutableArrayReferenceType (norm lex) (norm elemType) (map norm sizeExprs)
  norm (ReadonlyArrayReferenceDimType lex elemType dimExpr) =
    ReadonlyArrayReferenceDimType (norm lex) (norm elemType) (norm dimExpr)
  norm (MutableArrayReferenceDimType lex elemType dimExpr) =
    MutableArrayReferenceDimType (norm lex) (norm elemType) (norm dimExpr)

instance Normalizable DefcalTargetNode where
  norm (MeasureDefcalTarget lex) = MeasureDefcalTarget (norm lex)
  norm (ResetDefcalTarget lex) = ResetDefcalTarget (norm lex)
  norm (DelayDefcalTarget lex) = DelayDefcalTarget (norm lex)
  norm (IdentifierDefcalTarget lex) = IdentifierDefcalTarget (norm lex)

instance Normalizable DefcalArgumentDefinitionNode where
  norm (ExpressionDefcalArgument expr) = ExpressionDefcalArgument (norm expr)
  norm (ArgumentDefinitionDefcalArgument argDef) = ArgumentDefinitionDefcalArgument (norm argDef)

instance Normalizable DefcalOperandNode where
  norm (IdentifierDefcal lex) = IdentifierDefcal (norm lex)
  norm (HardwareQubitDefcal lex) = HardwareQubitDefcal (norm lex)

instance Normalizable GateOperandNode where
  norm (IdentifierGateOperand ident) = IdentifierGateOperand (norm ident)
  norm (HardwareQubitGateOperand lex) = HardwareQubitGateOperand (norm lex)

instance Normalizable ExternArgumentNode where
  norm (ScalarExternArgument sclType) = ScalarExternArgument (norm sclType)
  norm (ArrayExternArgument aryRefType) = ArrayExternArgument (norm aryRefType)
  norm (CregExternArgument lex maybeDsgn) = CregExternArgument (norm lex) ((Just . norm) =<< maybeDsgn)

instance Normalizable ArgumentDefinitionNode where
  norm (ScalarArgument sclType ident) = ScalarArgument (norm sclType) (norm ident)
  norm (QubitArgument qbType ident) = QubitArgument (norm qbType) (norm ident)
  norm (CregArgument lex ident maybeDsgn) = CregArgument (norm lex) (norm ident) ((Just . norm) =<< maybeDsgn)
  norm (QregArgument lex ident maybeDsgn) = QregArgument (norm lex) (norm ident) ((Just . norm) =<< maybeDsgn)
  norm (ArrayArgument aryRefType ident) = ArrayArgument (norm aryRefType) (norm ident)

instance Arbitrary Token where
  arbitrary =
    oneof
      [ return EofToken,
        return OpenqasmToken,
        return IncludeToken,
        return DefcalgrammarToken,
        return DefToken,
        return CalToken,
        return DefcalToken,
        return GateToken,
        return ExternToken,
        return BoxToken,
        return LetToken,
        return BreakToken,
        return ContinueToken,
        return IfToken,
        return ElseToken,
        return EndToken,
        return ReturnToken,
        return ForToken,
        return WhileToken,
        return InToken,
        return PragmaToken,
        liftM AnnotationKeywordToken arbitrary,
        return InputToken,
        return OutputToken,
        return ConstToken,
        return ReadonlyToken,
        return MutableToken,
        return QregToken,
        return QubitToken,
        return CregToken,
        return BoolToken,
        return BitToken,
        return IntToken,
        return UintToken,
        return FloatToken,
        return AngleToken,
        return ComplexToken,
        return ArrayToken,
        return VoidToken,
        return DurationToken,
        return StretchToken,
        return GphaseToken,
        return InvToken,
        return PowToken,
        return CtrlToken,
        return NegctrlToken,
        return DimToken,
        return DurationofToken,
        return DelayToken,
        return ResetToken,
        return MeasureToken,
        return BarrierToken,
        liftM BooleanLiteralToken arbitrary,
        return LbracketToken,
        return RbracketToken,
        return LbraceToken,
        return RbraceToken,
        return LparenToken,
        return RparenToken,
        return ColonToken,
        return SemicolonToken,
        return DotToken,
        return CommaToken,
        return EqualsToken,
        return ArrowToken,
        return PlusToken,
        return DoublePlusToken,
        return MinusToken,
        return AsteriskToken,
        return DoubleAsteriskToken,
        return SlashToken,
        return PercentToken,
        return PipeToken,
        return DoublePipeToken,
        return AmpersandToken,
        return DoubleAmpersandToken,
        return CaretToken,
        return AtToken,
        return TildeToken,
        return ExclamationPointToken,
        liftM EqualityOperatorToken arbitrary,
        liftM CompoundAssignmentOperatorToken arbitrary,
        liftM ComparisonOperatorToken arbitrary,
        liftM BitshiftOperatorToken arbitrary,
        liftM ImaginaryLiteralToken arbitrary,
        liftM BinaryIntegerLiteralToken arbitrary,
        liftM OctalIntegerLiteralToken arbitrary,
        liftM DecimalIntegerLiteralToken arbitrary,
        liftM HexIntegerLiteralToken arbitrary,
        liftM IdentifierToken arbitrary,
        liftM HardwareQubitToken arbitrary,
        liftM FloatLiteralToken arbitrary,
        liftM TimingLiteralToken arbitrary,
        liftM BitstringLiteralToken arbitrary,
        liftM WhitespaceToken arbitrary,
        return NewlineToken,
        liftM LineCommentToken arbitrary,
        liftM BlockCommentToken arbitrary,
        liftM VersionSpecifierToken arbitrary,
        liftM StringLiteralToken arbitrary,
        liftM RemainingLineContentToken arbitrary
      ]

instance Arbitrary Lexeme where
  arbitrary = liftM2 Lexeme (return Nothing) arbitrary

arbitraryLexeme :: (Monad m) => Token -> m Lexeme
arbitraryLexeme tok = return (Lexeme Nothing tok)

data ProgramConfig = ProgramConfig
  { depth :: (Int, Int),
    gates :: (Int, Int),
    gateArgs :: (Int, Int),
    aIdent :: Gen String
  }

arbitraryProgram :: Gen ProgramNode
arbitraryProgram = liftM3 Program (arbitraryLexeme OpenqasmToken {-version:-}) arbitrary {-programstmts:-} (return [])

{-

arbitraryStatementNode config =
    oneof
      [ -- Pragma (Lexeme PragmaToken) (Lexeme VersionSpecifierToken "3"),
        Annotated [] arbitrary
      ]

arbitraryAnnotationNode config =
    oneof
      [ Annotation Lexeme Lexeme
      ]

arbitraryStatementOrScopeNode config =
    oneof
      [ Statement arbitrary,
        Scope (sized (choose (0, 4)) arbitrary)
      ]

arbitraryStatementContentNode config =
    oneof
      [ -- LET Identifier (EQUALS) aliasExpression (SEMICOLON)
        AliasDeclaration Lexeme Lexeme AliasExpressionNode,
        -- indexedIdentifier assignmentOperator measureExpression (SEMICOLON)
        Assignment IndexedIdentifierNode Lexeme MeasureExpressionNode,
        -- BARRIER gateOperandList? (SEMICOLON)
        Barrier Lexeme [GateOperandNode],
        -- BOX designator? scope
        Box Lexeme (Maybe ExpressionNode) [StatementNode],
        -- BREAK (SEMICOLON)
        Break Lexeme,
        -- CAL (LBRACE) CalibrationBlock? (RBRACE)
        Cal Lexeme Lexeme,
        -- DEFCALGRAMMAR StringLiteral
        CalibrationGrammar Lexeme Lexeme,
        -- scalarOrArrayType Identifier ((EQUALS) declarationExpression)? (SEMICOLON)
        ClassicalDeclaration ScalarOrArrayTypeNode Lexeme (Maybe DeclarationExpressionNode),
        -- CONST scalarType Identifier (EQUALS) declarationExpression (SEMICOLON)
        ConstDeclaration Lexeme ScalarTypeNode Lexeme DeclarationExpressionNode,
        -- CONTINUE (SEMICOLON)
        Continue Lexeme,
        -- DEF Identifier (LPAREN) argumentDefinitionList (RPAREN) returnSignature? scope
        Def Lexeme Lexeme [ArgumentDefinitionNode] (Maybe ScalarTypeNode) [StatementNode],
        -- DEFCAL defcalTarget ((LPAREN) defcalArgumentDefinitionList (RPAREN))? defcalOperandList returnSignature? (LBRACE) CalibrationBlock? (RBRACE)
        Defcal Lexeme DefcalTargetNode [DefcalArgumentDefinitionNode] [DefcalOperandNode] (Maybe ScalarTypeNode) Lexeme,
        -- DELAY designator gateOperandList? (SEMICOLON)
        Delay Lexeme ExpressionNode [GateOperandNode],
        -- END (SEMICOLON)
        End Lexeme,
        -- expression (SEMICOLON)
        Expression ExpressionNode,
        -- EXTERN Identifier (LPAREN) externArgumentList (RPAREN) returnSignature? (SEMICOLON)
        Extern Lexeme Lexeme [ExternArgumentNode] (Maybe ScalarTypeNode),
        -- FOR scalarType Identifier (IN) expression statementOrScope
        For Lexeme ScalarTypeNode Lexeme ExpressionNode StatementOrScopeNode,
        -- FOR scalarType Identifier (IN) (LBRACKET) rangeExpression (RBRACKET) statementOrScope
        RangeFor Lexeme ScalarTypeNode Lexeme RangeExpressionNode StatementOrScopeNode,
        -- FOR scalarType Identifier (IN) setExpression statementOrScope
        SetFor Lexeme ScalarTypeNode Lexeme SetExpressionNode StatementOrScopeNode,
        -- GATE Identifier ((LPAREN) identifierList? (RPAREN))? identifierList scope
        Gate Lexeme Lexeme [Lexeme] [Lexeme] [StatementNode],
        -- gateModifierList Identifier ((LPAREN) expressionList (RPAREN))? designator? gateOperandList? (SEMICOLON)
        GateCall [GateModifierNode] Lexeme [ExpressionNode] (Maybe ExpressionNode) [GateOperandNode],
        -- IF (LPAREN) expression (RPAREN) statementOrScope ((ELSE) statementOrScope)?
        If Lexeme ExpressionNode StatementOrScopeNode (Maybe StatementOrScopeNode),
        -- INCLUDE StringLiteral (SEMICOLON)
        Include Lexeme Lexeme,
        -- INPUT scalarOrArrayType Identifier (SEMICOLON)
        InputIoDeclaration Lexeme ScalarOrArrayTypeNode Lexeme,
        -- OUTPUT scalarTypeOrArrayType Identifier (SEMICOLON)
        OutputIoDeclaration Lexeme ScalarOrArrayTypeNode Lexeme,
        -- MEASURE gateOperand ((ARROW) indexedIdentifier)? (SEMICOLON)
        MeasureArrowAssignment Lexeme GateOperandNode (Maybe IndexedIdentifierNode),
        -- CREG Identifier designator? (SEMICOLON)
        CregOldStyleDeclaration Lexeme Lexeme (Maybe ExpressionNode),
        -- QREG Identifier designator? (SEMICOLON)
        QregOldStyleDeclaration Lexeme Lexeme (Maybe ExpressionNode),
        -- qubitType Identifier (SEMICOLON)
        QuantumDeclaration QubitTypeNode Lexeme,
        -- RESET gateOperand (SEMICOLON)
        Reset Lexeme GateOperandNode,
        -- RETURN measureExpression? (SEMICOLON)
        Return Lexeme (Maybe MeasureExpressionNode),
        -- WHILE (LPAREN) expression (RPAREN) statementOrScope
        While Lexeme ExpressionNode StatementOrScopeNode
      ]

arbitraryScalarOrArrayTypeNode config =
    oneof
      [ Scalar ScalarTypeNode,
        Array ArrayTypeNode
      ]

arbitraryExpressionNode config =
    oneof
      [ ParenExpression ExpressionNode,
        IndexExpression ExpressionNode IndexOperatorNode,
        UnaryOperatorExpression Lexeme ExpressionNode,
        BinaryOperatorExpression ExpressionNode Lexeme ExpressionNode,
        CastExpression ScalarOrArrayTypeNode ExpressionNode,
        DurationOfExpression Lexeme [StatementNode],
        CallExpression Lexeme [ExpressionNode],
        Identifier Lexeme,
        BinaryIntegerLiteral Lexeme,
        OctalIntegerLiteral Lexeme,
        DecimalIntegerLiteral Lexeme,
        HexIntegerLiteral Lexeme,
        FloatLiteral Lexeme,
        ImaginaryLiteral Lexeme,
        BooleanLiteral Lexeme,
        BitstringLiteral Lexeme,
        TimingLiteral Lexeme,
        HardwareQubitLiteral Lexeme
      ]

arbitraryAliasExpressionNode config =
    oneof
      [ AliasExpression [ExpressionNode]
      ]

arbitraryDeclarationExpressionNode config =
    oneof
      [ ArrayLiteralDeclarationExpression ArrayLiteralNode,
        ExpressionDeclarationExpression MeasureExpressionNode
      ]

arbitraryMeasureExpressionNode config =
    oneof
      [ PlainExpression ExpressionNode,
        MeasureExpression Lexeme GateOperandNode
      ]

arbitraryRangeOrExpressionIndexNode config =
    oneof
      [ ExpressionIndex ExpressionNode,
        RangeIndex RangeExpressionNode
      ]

arbitraryRangeExpressionNode config =
    oneof
      [ RangeExpression (Maybe SourceRef) (Maybe ExpressionNode) (Maybe ExpressionNode) (Maybe ExpressionNode)
      ]

arbitrarySetExpressionNode config =
    oneof
      [ SetExpression [ExpressionNode]
      ]

arbitraryArrayLiteralNode config =
    oneof
      [ ArrayLiteral [ArrayLiteralElementNode]
      ]

arbitraryArrayLiteralElementNode config =
    oneof
      [ ExpressionArrayElement ExpressionNode,
        ArrayArrayElement ArrayLiteralNode
      ]

arbitraryIndexOperatorNode config =
    oneof
      [ SetIndex SetExpressionNode,
        IndexList [RangeOrExpressionIndexNode]
      ]

arbitraryIndexedIdentifierNode config =
    oneof
      [ IndexedIdentifier Lexeme [IndexOperatorNode]
      ]

arbitraryGateModifierNode config =
    oneof
      [ InvGateModifier Lexeme,
        PowGateModifier Lexeme ExpressionNode,
        CtrlGateModifier Lexeme (Maybe ExpressionNode),
        NegCtrlGateModifier Lexeme (Maybe ExpressionNode)
      ]

arbitraryScalarTypeNode config =
    oneof
      [ BitType Lexeme (Maybe ExpressionNode),
        IntType Lexeme (Maybe ExpressionNode),
        UintType Lexeme (Maybe ExpressionNode),
        FloatType Lexeme (Maybe ExpressionNode),
        AngleType Lexeme (Maybe ExpressionNode),
        BoolType Lexeme,
        DurationType Lexeme,
        StretchType Lexeme,
        ComplexType Lexeme (Maybe ScalarTypeNode)
      ]

arbitraryQubitTypeNode config =
    oneof
      [ QubitType Lexeme (Maybe ExpressionNode)
      ]

arbitraryArrayTypeNode config =
    oneof
      [ ArrayType Lexeme ScalarTypeNode [ExpressionNode]
      ]

arbitraryArrayReferenceTypeNode config =
    oneof
      [ ReadonlyArrayReferenceType Lexeme ScalarTypeNode [ExpressionNode],
        MutableArrayReferenceType Lexeme ScalarTypeNode [ExpressionNode],
        ReadonlyArrayReferenceDimType Lexeme ScalarTypeNode ExpressionNode,
        MutableArrayReferenceDimType Lexeme ScalarTypeNode ExpressionNode
      ]

-- Start miscellany.

-- DesignatorNode elided, use ExpressionNode.

arbitraryDefcalTargetNode config =
    oneof
      [ MeasureDefcalTarget Lexeme,
        ResetDefcalTarget Lexeme,
        DelayDefcalTarget Lexeme,
        IdentifierDefcalTarget Lexeme
      ]

arbitraryDefcalArgumentDefinitionNode config =
    oneof
      [ ExpressionDefcalArgument ExpressionNode,
        ArgumentDefinitionDefcalArgument ArgumentDefinitionNode
      ]

arbitraryDefcalOperandNode config =
    oneof
      [ IdentifierDefcal Lexeme,
        HardwareQubitDefcal Lexeme
      ]

arbitraryGateOperandNode config =
    oneof
      [ IdentifierGateOperand IndexedIdentifierNode,
        HardwareQubitGateOperand Lexeme
      ]

arbitraryExternArgumentNode config =
    oneof
      [ ScalarExternArgument ScalarTypeNode,
        ArrayExternArgument ArrayReferenceTypeNode,
        CregExternArgument Lexeme (Maybe ExpressionNode)
      ]

arbitraryArgumentDefinitionNode config =
    oneof
      [ -- scalarType Identifier
        ScalarArgument ScalarTypeNode Lexeme,
        -- qubitType Identifier
        QubitArgument QubitTypeNode Lexeme,
        -- CREG Identifier designator?
        CregArgument Lexeme Lexeme (Maybe ExpressionNode),
        -- QREG Identifier designator?
        QregArgument Lexeme Lexeme (Maybe ExpressionNode),
        -- arrayReferenceType Identifier
        ArrayArgument ArrayReferenceTypeNode Lexeme
      ]

-- -}