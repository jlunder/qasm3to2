{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Evaluate" #-}
module Qasm3.Test.Arbitrary where

import Ast
import Control.Monad
import Data.Bits
import Numeric (showBin, showHex, showOct)
import Qasm3
import Test.QuickCheck

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
      aliasArgumentsSizeRange = (1, 2),
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
      annotationKeyword = elements $ map ('@' :) ["reversible", "crosstalk", "noise", "noswap", "e29_12", "_34rq"],
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

arbitraryProgramNode :: ProgramConfig -> Gen (AstNode Tag ())
arbitraryProgramNode cfg = do
  sz <- chooseInt (scopeSizeRange cfg)
  stmts <- vectorOf sz (arbitraryStatementNode cfg)
  let tok = VersionSpecifierToken "3"
  let (maj, min) = tokenVersionMajMin tok
  return $ AstNode (Program maj min tok) stmts ()

arbitraryAnnotationNode :: ProgramConfig -> Gen (AstNode Tag ())
arbitraryAnnotationNode cfg = do
  kw <- annotationKeyword cfg
  p <- annotationParam cfg
  return $ AstNode (Annotation (tokenIdentifierName (AnnotationKeywordToken kw)) p (AnnotationKeywordToken kw)) [] ()

arbitraryAnnotationNodeList :: ProgramConfig -> Gen [AstNode Tag ()]
arbitraryAnnotationNodeList cfg = do
  sz <- chooseInt (annotationListSizeRange cfg)
  vectorOf sz (arbitraryAnnotationNode cfg)

arbitraryStatementOrScopeNode :: ProgramConfig -> Gen (AstNode Tag ())
arbitraryStatementOrScopeNode cfg =
  do
    let d = statementDepth cfg
    depthMax <- chooseInt (statementDepthRange cfg)
    sz <- if d < depthMax then chooseInt (scopeSizeRange cfg) else return 0
    oneof
      [ arbitraryStatementNode cfg,
        (\stmts -> AstNode Scope stmts ()) <$> vectorOf sz (arbitraryStatementNode cfg)
      ]

astList :: [AstNode Tag ()] -> AstNode Tag ()
astList elems = AstNode List elems ()

astIdent :: String -> AstNode Tag ()
astIdent identName =
  let tok = IdentifierToken identName
   in AstNode (Identifier (tokenIdentifierName tok) tok) [] ()

arbitraryIdentifier :: Gen String -> Gen (AstNode Tag ())
arbitraryIdentifier identGen =
  let nodeGen identName =
        let tok = IdentifierToken identName
         in AstNode (Identifier (tokenIdentifierName tok) tok) [] ()
   in nodeGen <$> identGen

arbitraryStatementNode :: ProgramConfig -> Gen (AstNode Tag ())
arbitraryStatementNode cfg = do
  annotations <- arbitraryAnnotationNodeList cfg
  statement <- arbitraryStatementContentNode cfg
  return $ AstNode Statement (statement : annotations) ()

arbitraryStatementContentNode :: ProgramConfig -> Gen (AstNode Tag ())
arbitraryStatementContentNode cfg =
  oneof
    [ -- LET Identifier (EQUALS) aliasExpression (SEMICOLON)
      arbitraryAliasExpressionNode cfg
      {--
      -- indexedIdentifier assignmentOperator measureExpression (SEMICOLON)
      AssignmentStmt IndexedIdentifierNode Lexeme MeasureExpressionNode,
      -- BARRIER gateOperandList? (SEMICOLON)
      BarrierStmt Lexeme [GateOperandNode],
      -- BOX designator? scope
      BoxStmt Lexeme (Maybe ExpressionNode) [StatementNode],
      -- BREAK (SEMICOLON)
      BreakStmt Lexeme,
      -- CAL (LBRACE) CalibrationBlock? (RBRACE)
      CalStmt Lexeme Lexeme,
      -- DEFCALGRAMMAR StringLiteral
      CalibrationGrammar Lexeme Lexeme,
      -- scalarOrArrayType Identifier ((EQUALS) declarationExpression)? (SEMICOLON)
      ClassicalDeclaration ScalarOrArrayTypeNode Lexeme (Maybe DeclarationExpressionNode),
      -- CONST scalarType Identifier (EQUALS) declarationExpression (SEMICOLON)
      ConstDeclaration Lexeme ScalarTypeNode Lexeme DeclarationExpressionNode,
      -- CONTINUE (SEMICOLON)
      ContinueStmt Lexeme,
      -- DEF Identifier (LPAREN) argumentDefinitionList (RPAREN) returnSignature? scope
      DefStmt Lexeme Lexeme [ArgumentDefinitionNode] (Maybe ScalarTypeNode) [StatementNode],
      -- DEFCAL defcalTarget ((LPAREN) defcalArgumentDefinitionList (RPAREN))? defcalOperandList returnSignature? (LBRACE) CalibrationBlock? (RBRACE)
      DefcalStmt Lexeme DefcalTargetNode [DefcalArgumentDefinitionNode] [DefcalOperandNode] (Maybe ScalarTypeNode) Lexeme,
      -- DELAY designator gateOperandList? (SEMICOLON)
      DelayStmt Lexeme ExpressionNode [GateOperandNode],
      -- END (SEMICOLON)
      EndStmt Lexeme,
      -- expression (SEMICOLON)
      Expression ExpressionNode,
      -- EXTERN Identifier (LPAREN) externArgumentList (RPAREN) returnSignature? (SEMICOLON)
      ExternStmt Lexeme Lexeme [ExternArgumentNode] (Maybe ScalarTypeNode),
      -- FOR scalarType Identifier (IN) expression statementOrScope
      ForStmt Lexeme ScalarTypeNode Lexeme ExpressionNode StatementOrScopeNode,
      -- FOR scalarType Identifier (IN) (LBRACKET) rangeExpression (RBRACKET) statementOrScope
      RangeFor Lexeme ScalarTypeNode Lexeme RangeExpressionNode StatementOrScopeNode,
      -- FOR scalarType Identifier (IN) setExpression statementOrScope
      SetFor Lexeme ScalarTypeNode Lexeme SetExpressionNode StatementOrScopeNode,
      -- GATE Identifier ((LPAREN) identifierList? (RPAREN))? identifierList scope
      GateStmt Lexeme Lexeme [Lexeme] [Lexeme] [StatementNode],
      -- gateModifierList Identifier ((LPAREN) expressionList (RPAREN))? designator? gateOperandList? (SEMICOLON)
      GateCallStmt [GateModifierNode] Lexeme [ExpressionNode] (Maybe ExpressionNode) [GateOperandNode],
      -- IF (LPAREN) expression (RPAREN) statementOrScope ((ELSE) statementOrScope)?
      IfStmt Lexeme ExpressionNode StatementOrScopeNode (Maybe StatementOrScopeNode),
      -- INCLUDE StringLiteral (SEMICOLON)
      IncludeStmt Lexeme Lexeme,
      -- INPUT scalarOrArrayType Identifier (SEMICOLON)
      InputIoDeclaration Lexeme ScalarOrArrayTypeNode Lexeme,
      -- OUTPUT scalarTypeOrArrayType Identifier (SEMICOLON)
      OutputIoDeclaration Lexeme ScalarOrArrayTypeNode Lexeme,
      -- MEASURE gateOperand ((ARROW) indexedIdentifier)? (SEMICOLON)
      MeasureArrowAssignmentStmt Lexeme GateOperandNode (Maybe IndexedIdentifierNode),
      -- CREG Identifier designator? (SEMICOLON)
      CregOldStyleDeclaration Lexeme Lexeme (Maybe ExpressionNode),
      -- QREG Identifier designator? (SEMICOLON)
      QregOldStyleDeclaration Lexeme Lexeme (Maybe ExpressionNode),
      -- qubitType Identifier (SEMICOLON)
      QuantumDeclaration QubitTypeNode Lexeme,
      -- RESET gateOperand (SEMICOLON)
      ResetStmt Lexeme GateOperandNode,
      -- RETURN measureExpression? (SEMICOLON)
      ReturnStmt Lexeme (Maybe MeasureExpressionNode),
      -- WHILE (LPAREN) expression (RPAREN) statementOrScope
      WhileStmt Lexeme ExpressionNode StatementOrScopeNode
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

{-
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
-}

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
  lenRange <- elements [3, 6, 9, 12, 15]
  numStr <- vectorOf lenRange (choose ('0', '7'))
  return ("0o" ++ numStr)

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

arbitraryIntExpressionNode :: ProgramConfig -> Gen (AstNode Tag ())
arbitraryIntExpressionNode cfg =
  oneof
    [ (\expr -> AstNode ParenExpr [expr] ()) <$> arbitraryIntExpressionNode cfg,
      (\op left right -> AstNode (BinaryOperatorExpr op) [left, right] ())
        <$> arbitraryIntBinaryOp
        <*> arbitraryIntExpressionNode cfg
        <*> arbitraryIntExpressionNode cfg,
      (\op expr -> AstNode (UnaryOperatorExpr op) [expr] ())
        <$> arbitraryIntUnaryOp
        <*> arbitraryIntExpressionNode cfg,
      (\tok -> AstNode (Identifier (tokenIdentifierName tok) tok) [] ()) . IdentifierToken <$> constIdent cfg,
      (\tok -> AstNode (IntegerLiteral (tokenIntegerVal tok) tok) [] ()) . BinaryIntegerLiteralToken
        <$> arbitraryBinaryLiteralString,
      (\tok -> AstNode (IntegerLiteral (tokenIntegerVal tok) tok) [] ()) . OctalIntegerLiteralToken
        <$> arbitraryOctalLiteralString,
      (\tok -> AstNode (IntegerLiteral (tokenIntegerVal tok) tok) [] ()) . DecimalIntegerLiteralToken
        <$> arbitraryDecimalLiteralString,
      (\tok -> AstNode (IntegerLiteral (tokenIntegerVal tok) tok) [] ()) . HexIntegerLiteralToken
        <$> arbitraryHexLiteralString
    ]

arbitraryIntUnaryOp :: Gen Token
arbitraryIntUnaryOp = elements [TildeToken, ExclamationPointToken, MinusToken]

arbitraryIntBinaryOp :: Gen Token
arbitraryIntBinaryOp =
  elements
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
      DoubleLessToken,
      DoubleGreaterToken
    ]

arbitraryAliasExpressionNode :: ProgramConfig -> Gen (AstNode Tag ())
arbitraryAliasExpressionNode cfg = do
  ident <- arbitraryIdentifier (constIdent cfg)
  sz <- chooseInt (aliasArgumentsSizeRange cfg)
  elements <- vectorOf sz (arbitraryAliasElement cfg)
  return $ AstNode AliasDeclStmt (ident : elements) ()

arbitraryAliasElement :: ProgramConfig -> Gen (AstNode Tag ())
arbitraryAliasElement cfg = do
  identFn <- elements [classicalIdent cfg, qubitIdent cfg]
  let identGen = arbitraryIdentifier identFn
  oneof [identGen, arbitraryIndexOperatorNode cfg identGen]

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

arbitraryRangeExpressionNode :: ProgramConfig -> Gen (AstNode Tag ())
arbitraryRangeExpressionNode cfg = do
  begin <- oneof [return NilNode, arbitraryIntExpressionNode cfg]
  step <- oneof [return NilNode, arbitraryIntExpressionNode cfg]
  end <- oneof [return NilNode, arbitraryIntExpressionNode cfg]
  return $ AstNode RangeInitExpr [begin, step, end] ()

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

arbitraryIndexOperatorNode :: ProgramConfig -> Gen (AstNode Tag ()) -> Gen (AstNode Tag ())
arbitraryIndexOperatorNode cfg exprGen = do
  expr <- exprGen
  index <-
    oneof
      [ (\e -> AstNode SetInitExpr e ()) <$> resize 10 (listOf $ arbitraryIntExpressionNode cfg),
        arbitraryIntExpressionNode cfg,
        arbitraryRangeExpressionNode cfg
      ]
  return $ AstNode IndexExpr [expr, index] ()

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