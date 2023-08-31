{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Evaluate" #-}
module Qasm3Arbitrary where

import Control.Monad
import Data.Bits
import Numeric (showBin, showHex, showOct)
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
  norm (IntegerLiteral lit) = IntegerLiteral (norm lit)
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

normLex :: Token -> Lexeme
normLex = Lexeme Nothing

reservedKeywords =
  [ "def",
    "gate",
    "extern",
    "box",
    "let",
    "break",
    "continue",
    "if",
    "else",
    "end",
    "return",
    "for",
    "while",
    "in",
    "input",
    "output",
    "const",
    "readonly",
    "mutable",
    "qreg",
    "qubit",
    "creg",
    "bool",
    "bit",
    "int",
    "uint",
    "float",
    "angle",
    "complex",
    "array",
    "void",
    "duration",
    "stretch",
    "gphase",
    "inv",
    "pow",
    "ctrl",
    "negctrl",
    "durationof",
    "delay",
    "reset",
    "measure",
    "barrier",
    "true",
    "false",
    "defcalgrammar",
    "cal",
    "defcal"
  ]

data ProgramConfig = ProgramConfig
  { statementDepth :: Int,
    expressionDepth :: Int,
    statementDepthRange :: (Int, Int),
    expressionDepthRange :: (Int, Int),
    indexDimRange :: (Int, Int),
    scopeSizeRange :: (Int, Int),
    annotationListSizeRange :: (Int, Int),
    aliasArgumentsSizeRange :: (Int, Int),
    gateArgumentsSizeRange :: (Int, Int),
    pragmaParam :: Gen String,
    annotationKeyword :: Gen String,
    annotationParam :: Gen String,
    gateIdent :: Gen String,
    qubitIdent :: Gen String,
    bitIdent :: Gen String,
    constIdent :: Gen String,
    classicalIdent :: Gen String
  }

defaultConfig =
  ProgramConfig
    { statementDepth = undefined,
      expressionDepth = undefined,
      statementDepthRange = (4, 6),
      expressionDepthRange = (1, 8),
      indexDimRange = (1, 3),
      scopeSizeRange = (0, 8),
      annotationListSizeRange = (0, 2),
      aliasArgumentsSizeRange = (0, 2),
      gateArgumentsSizeRange = (0, 5),
      pragmaParam =
        oneof
          [ do
              un <- elements ["alice", "bob", "carol", "dan", "edith", "frank", "gertrude", "harry"]
              uid <- chooseInt (100, 1000000)
              return ("user " ++ un ++ " account " ++ show uid),
            do
              temp <- chooseInt (4, 9)
              return ("max_temp qpu 0." ++ show temp)
          ],
      annotationKeyword = elements ["reversible", "crosstalk", "noise", "noswap", "e29_12", "_34rq"],
      annotationParam =
        frequency
          [ (7, return ""),
            (1, return "\" some test"),
            (1, return "IOPORT[3:2]"),
            (1, return "{unbalan'ced ( [ < $# // not a comment")
          ],
      gateIdent = arbitraryIdentStr 'g',
      qubitIdent = arbitraryIdentStr 'q',
      bitIdent = arbitraryIdentStr 'b',
      constIdent = arbitraryIdentStr 'c',
      classicalIdent = arbitraryIdentStr 'v'
    }

arbitraryIdentStr :: Char -> Gen [Char]
arbitraryIdentStr leadCh =
  suchThat
    ( oneof
        [ do
            i <- chooseInt (0, 99999)
            return (leadCh : show i),
          do
            lu <- chooseInt (0, 1)
            ls <- chooseInt (0, 8)
            u <- vectorOf lu (return '_')
            s <- vectorOf ls arbitraryIdentChar
            return (u ++ [leadCh] ++ s)
        ]
    )
    (`notElem` reservedKeywords)

arbitraryLeadIdentChar :: Gen Char
arbitraryLeadIdentChar = frequency [(26, choose ('a', 'z')), (26, choose ('A', 'Z')), (10, return '_')]

arbitraryIdentChar :: Gen Char
arbitraryIdentChar =
  frequency
    [ (260, choose ('a', 'z')),
      (260, choose ('A', 'Z')),
      (10, choose ('0', '9')),
      (20, return '_')
    ]

arbitraryProgramNode :: ProgramConfig -> Gen ProgramNode
arbitraryProgramNode cfg = do
  sz <- chooseInt (scopeSizeRange cfg)
  stmts <- vectorOf (sz `div` 2) (arbitraryStatementNode cfg)
  return $ Program (normLex OpenqasmToken) (normLex (VersionSpecifierToken "3")) stmts

arbitraryAnnotationNode :: ProgramConfig -> Gen AnnotationNode
arbitraryAnnotationNode cfg = do
  kw <- annotationKeyword cfg
  p <- annotationParam cfg
  return
    ( Annotation
        (normLex $ AnnotationKeywordToken kw)
        (normLex $ RemainingLineContentToken p)
    )

arbitraryAnnotationNodeList :: ProgramConfig -> Gen [AnnotationNode]
arbitraryAnnotationNodeList cfg = do
  sz <- chooseInt (annotationListSizeRange cfg)
  vectorOf sz (arbitraryAnnotationNode cfg)

arbitraryStatementOrScopeNode :: ProgramConfig -> Gen StatementOrScopeNode
arbitraryStatementOrScopeNode cfg =
  do
    let d = statementDepth cfg
    depthMax <- chooseInt (statementDepthRange cfg)
    sz <- if d < depthMax then chooseInt (scopeSizeRange cfg) else return 0
    oneof
      [ Statement <$> arbitraryStatementNode cfg,
        Scope <$> vectorOf sz (arbitraryStatementNode cfg)
      ]

arbitraryStatementNode :: ProgramConfig -> Gen StatementNode
arbitraryStatementNode cfg = do
  annotations <- arbitraryAnnotationNodeList cfg
  statement <- arbitraryStatementContentNode cfg
  return $ Annotated annotations statement

arbitraryStatementContentNode :: ProgramConfig -> Gen StatementContentNode
arbitraryStatementContentNode cfg =
  oneof
    [ -- LET Identifier (EQUALS) aliasExpression (SEMICOLON)
      do
        ident <- bitIdent cfg
        aliasExpr <- arbitraryAliasExpressionNode cfg
        return $ AliasDeclaration (normLex LetToken) (normLex $ IdentifierToken ident) aliasExpr
        {--
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
        --}
    ]

{-

arbitraryScalarOrArrayTypeNode cfg =
    oneof
      [ Scalar ScalarTypeNode,
        Array ArrayTypeNode
      ]
-}

{-
arbitraryExpressionNode cfg =
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
-}

arbitraryIdentifierLexeme :: Gen String -> Gen Lexeme
arbitraryIdentifierLexeme tokenStr = normLex . IdentifierToken <$> tokenStr

arbitraryBinaryLiteralLexeme :: Gen Lexeme
arbitraryBinaryLiteralLexeme = normLex . BinaryIntegerLiteralToken <$> arbitraryBinaryLiteralString

arbitraryOctalLiteralLexeme :: Gen Lexeme
arbitraryOctalLiteralLexeme = normLex . OctalIntegerLiteralToken <$> arbitraryOctalLiteralString

arbitraryDecimalLiteralLexeme :: Gen Lexeme
arbitraryDecimalLiteralLexeme = normLex . DecimalIntegerLiteralToken <$> arbitraryDecimalLiteralString

arbitraryHexLiteralLexeme :: Gen Lexeme
arbitraryHexLiteralLexeme = normLex . HexIntegerLiteralToken <$> arbitraryHexLiteralString

arbitraryFloatLiteralLexeme :: Gen Lexeme
arbitraryFloatLiteralLexeme = normLex . FloatLiteralToken <$> arbitraryFloatLiteralString

rangeBinaryLiteralString :: (Integral a) => Gen a -> Gen String
rangeBinaryLiteralString litGen = do
  prefix <- elements ["0b", "0B"]
  numStr <- showBin <$> litGen
  return (prefix ++ numStr "")

arbitraryBinaryLiteralString :: Gen String
arbitraryBinaryLiteralString = do
  prefix <- elements ["0b", "0B"]
  lenRange <- elements [8, 16, 32, 64, 128]
  numStr <- vectorOf lenRange (choose ('0', '1'))
  return (prefix ++ numStr)

rangeOctalLiteralString :: (Integral a) => Gen a -> Gen String
rangeOctalLiteralString litGen = ("0o" ++) <$> ((`showOct` "") <$> litGen)

arbitraryOctalLiteralString :: Gen String
arbitraryOctalLiteralString = do
  prefix <- elements ["0b", "0B"]
  lenRange <- elements [3, 6, 9, 12, 15]
  numStr <- vectorOf lenRange (choose ('0', '7'))
  return (prefix ++ numStr)

rangeDecimalLiteralString :: (Integral a, Show a) => Gen a -> Gen String
rangeDecimalLiteralString litGen = show <$> litGen

arbitraryDecimalLiteralString :: Gen String
arbitraryDecimalLiteralString = do
  lenRange <- elements [3, 6, 9, 12, 15]
  vectorOf lenRange (choose ('0', '9'))

rangeHexLiteralString :: (Integral a) => Gen a -> Gen String
rangeHexLiteralString litGen = do
  prefix <- elements ["0x", "0X"]
  numStr <- showHex <$> litGen
  return (prefix ++ numStr "")

arbitraryHexLiteralString :: Gen String
arbitraryHexLiteralString = do
  prefix <- elements ["0x", "0X"]
  lenRange <- elements [2, 4, 8, 16, 32]
  numStr <- vectorOf lenRange (frequency [(20, choose ('0', '9')), (6, choose ('a', 'f')), (6, choose ('A', 'F'))])
  return (prefix ++ numStr)

arbitraryFloatLiteralString :: Gen String
arbitraryFloatLiteralString =
  let decPart :: Gen String -> Gen String
      decPart g = (++) <$> arbitraryDecimalLiteralString <*> g
      dotPart :: Gen String -> Gen String
      dotPart g = (('.' :) <$> g)
      expPart :: Gen String -> Gen String
      expPart g = (++) <$> (('e' :) <$> elements ["+", "-", ""]) <*> g
      end :: Gen String
      end = return ""
   in oneof
        [ dotPart $ decPart end,
          dotPart $ decPart $ expPart $ decPart end,
          decPart $ dotPart end,
          decPart $ expPart end,
          decPart $ dotPart $ expPart end,
          decPart $ dotPart $ decPart end,
          decPart $ dotPart $ decPart $ expPart end
        ]

arbitraryImaginaryLiteralString :: Gen String
arbitraryImaginaryLiteralString = do
  floatStr <- arbitraryFloatLiteralString
  spaceStr <- resize 5 $ listOf $ elements [' ', '\t']
  return $ concat [floatStr, spaceStr, "im"]

arbitraryBooleanLiteralString :: Gen String
arbitraryBooleanLiteralString = elements ["true", "false"]

arbitraryBitstringLiteralString :: Gen String
arbitraryBitstringLiteralString = do
  lenRange <- elements [8, 16, 32, 64, 128]
  bitStr <- vectorOf lenRange (choose ('0', '1'))
  return ('"' : bitStr ++ "\"")

arbitraryHardwareQubitLiteralString :: Gen String
arbitraryHardwareQubitLiteralString = do
  numStr <- vectorOf 3 (choose ('0', '9'))
  return ('$' : numStr)

arbitraryTimingLiteralString :: Gen String
arbitraryTimingLiteralString = do
  timeStr <- oneof [arbitraryDecimalLiteralString, arbitraryFloatLiteralString]
  numSpaces <- choose (0, 3)
  spaceStr <- vectorOf numSpaces (elements [' ', '\t'])
  unitStr <- elements ["dt", "ns", "us", "µs", "ms", "s"]
  return (timeStr ++ spaceStr ++ unitStr)

arbitraryIntExpressionNode :: ProgramConfig -> Gen ExpressionNode
arbitraryIntExpressionNode cfg =
  oneof
    [ ParenExpression <$> arbitraryIntExpressionNode cfg,
      UnaryOperatorExpression <$> arbitraryIntUnaryOp <*> arbitraryIntExpressionNode cfg,
      BinaryOperatorExpression <$> arbitraryIntExpressionNode cfg <*> arbitraryIntBinaryOp <*> arbitraryIntExpressionNode cfg,
      Identifier <$> arbitraryIdentifierLexeme (constIdent cfg),
      IntegerLiteral <$> arbitraryBinaryLiteralLexeme,
      IntegerLiteral <$> arbitraryOctalLiteralLexeme,
      IntegerLiteral <$> arbitraryDecimalLiteralLexeme,
      IntegerLiteral <$> arbitraryHexLiteralLexeme
    ]

arbitraryIntUnaryOp :: Gen Lexeme
arbitraryIntUnaryOp =
  elements $ map normLex [TildeToken, ExclamationPointToken, MinusToken]

arbitraryIntBinaryOp :: Gen Lexeme
arbitraryIntBinaryOp =
  elements $
    map
      normLex
      [ PlusToken,
        MinusToken,
        AsteriskToken,
        DoubleAsteriskToken,
        SlashToken,
        PercentToken,
        PipeToken,
        DoublePipeToken,
        AmpersandToken,
        DoubleAmpersandToken,
        CaretToken,
        AtToken,
        BitshiftOperatorToken "<<",
        BitshiftOperatorToken ">>"
      ]

arbitraryAliasExpressionNode :: ProgramConfig -> Gen AliasExpressionNode
arbitraryAliasExpressionNode cfg = do
  sz <- chooseInt (gateArgumentsSizeRange cfg)
  elements <- vectorOf sz (arbitraryAliasElement cfg)
  return $ AliasExpression elements

arbitraryAliasElement :: ProgramConfig -> Gen ExpressionNode
arbitraryAliasElement cfg = do
  ident <- elements [classicalIdent cfg, qubitIdent cfg]
  let identExprGen = Identifier . normLex . IdentifierToken <$> ident
  oneof
    [ identExprGen,
      IndexExpression <$> identExprGen <*> arbitraryIndexOperatorNode cfg
    ]

-- arbitraryArgumentExpression sz cfg = do
--   return vectorOf (arbitraryAlias)

{--
arbitraryDeclarationExpressionNode cfg =
    oneof
      [ ArrayLiteralDeclarationExpression ArrayLiteralNode,
        ExpressionDeclarationExpression MeasureExpressionNode
      ]

arbitraryMeasureExpressionNode cfg =
    oneof
      [ PlainExpression ExpressionNode,
        MeasureExpression Lexeme GateOperandNode
      ]
-}

arbitraryRangeOrExpressionIndexNode :: ProgramConfig -> Gen RangeOrExpressionIndexNode
arbitraryRangeOrExpressionIndexNode cfg =
  oneof
    [ ExpressionIndex <$> arbitraryIntExpressionNode cfg,
      RangeIndex <$> arbitraryRangeExpressionNode cfg
    ]

insideMaybe :: Maybe (Gen a) -> Gen (Maybe a)
insideMaybe Nothing = return Nothing
insideMaybe (Just g) = Just <$> g

arbitraryRangeExpressionNode :: ProgramConfig -> Gen RangeExpressionNode
arbitraryRangeExpressionNode cfg = do
  genStart <- elements [Nothing, Just (arbitraryIntExpressionNode cfg)]
  genEnd <- elements [Nothing, Just (arbitraryIntExpressionNode cfg)]
  genStep <- elements [Nothing, Just (arbitraryIntExpressionNode cfg)]
  start <- insideMaybe genStart
  end <- insideMaybe genEnd
  step <- insideMaybe genStep
  return $ RangeExpression Nothing start end step

-- TODO
arbitrarySetExpressionNode :: ProgramConfig -> Gen SetExpressionNode
arbitrarySetExpressionNode cfg = do
  setIndices <- resize 10 (listOf $ arbitraryIntExpressionNode cfg)
  return $ SetExpression setIndices

{-
arbitraryArrayLiteralNode cfg =
    oneof
      [ ArrayLiteral [ArrayLiteralElementNode]
      ]

arbitraryArrayLiteralElementNode cfg =
    oneof
      [ ExpressionArrayElement ExpressionNode,
        ArrayArrayElement ArrayLiteralNode
      ]
-}

arbitraryIndexOperatorNode :: ProgramConfig -> Gen IndexOperatorNode
arbitraryIndexOperatorNode cfg = do
  oneof
    [ SetIndex <$> arbitrarySetExpressionNode cfg,
      do
        dim <- chooseInt $ indexDimRange cfg
        IndexList <$> vectorOf dim (arbitraryRangeOrExpressionIndexNode cfg)
    ]

--
-- ExpressionIndex ExpressionNode
-- RangeIndex RangeExpressionNode

-- SetIndex SetExpressionNode,
-- IndexList [RangeOrExpressionIndexNode]

{-
arbitraryIndexedIdentifierNode cfg =
    oneof
      [ IndexedIdentifier Lexeme [IndexOperatorNode]
      ]

arbitraryGateModifierNode cfg =
    oneof
      [ InvGateModifier Lexeme,
        PowGateModifier Lexeme ExpressionNode,
        CtrlGateModifier Lexeme (Maybe ExpressionNode),
        NegCtrlGateModifier Lexeme (Maybe ExpressionNode)
      ]

arbitraryScalarTypeNode cfg =
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

arbitraryQubitTypeNode cfg =
    oneof
      [ QubitType Lexeme (Maybe ExpressionNode)
      ]

arbitraryArrayTypeNode cfg =
    oneof
      [ ArrayType Lexeme ScalarTypeNode [ExpressionNode]
      ]

arbitraryArrayReferenceTypeNode cfg =
    oneof
      [ ReadonlyArrayReferenceType Lexeme ScalarTypeNode [ExpressionNode],
        MutableArrayReferenceType Lexeme ScalarTypeNode [ExpressionNode],
        ReadonlyArrayReferenceDimType Lexeme ScalarTypeNode ExpressionNode,
        MutableArrayReferenceDimType Lexeme ScalarTypeNode ExpressionNode
      ]

-- Start miscellany.

-- DesignatorNode elided, use ExpressionNode.

arbitraryDefcalTargetNode cfg =
    oneof
      [ MeasureDefcalTarget Lexeme,
        ResetDefcalTarget Lexeme,
        DelayDefcalTarget Lexeme,
        IdentifierDefcalTarget Lexeme
      ]

arbitraryDefcalArgumentDefinitionNode cfg =
    oneof
      [ ExpressionDefcalArgument ExpressionNode,
        ArgumentDefinitionDefcalArgument ArgumentDefinitionNode
      ]

arbitraryDefcalOperandNode cfg =
    oneof
      [ IdentifierDefcal Lexeme,
        HardwareQubitDefcal Lexeme
      ]

arbitraryGateOperandNode cfg =
    oneof
      [ IdentifierGateOperand IndexedIdentifierNode,
        HardwareQubitGateOperand Lexeme
      ]

arbitraryExternArgumentNode cfg =
    oneof
      [ ScalarExternArgument ScalarTypeNode,
        ArrayExternArgument ArrayReferenceTypeNode,
        CregExternArgument Lexeme (Maybe ExpressionNode)
      ]

arbitraryArgumentDefinitionNode cfg =
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