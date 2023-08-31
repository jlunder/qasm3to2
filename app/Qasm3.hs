{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE InstanceSigs #-}

module Qasm3 where

import Ast
import Control.Monad
import Data.List (intercalate)
import Data.Maybe (listToMaybe)

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
  | NewlineToken
  | LineCommentToken String
  | BlockCommentToken String
  | VersionSpecifierToken String
  | StringLiteralToken String
  | RemainingLineContentToken String
  | CalibrationBlockToken String
  deriving (Eq, Ord, Read, Show)

instance AstNode Token where
  sourceRef _ = Nothing

  pretty EofToken = undefined
  pretty OpenqasmToken = "OPENQASM"
  pretty IncludeToken = "include"
  pretty DefcalgrammarToken = "defcalgrammar"
  pretty DefToken = "def"
  pretty CalToken = "cal"
  pretty DefcalToken = "defcal"
  pretty GateToken = "gate"
  pretty ExternToken = "extern"
  pretty BoxToken = "box"
  pretty LetToken = "let"
  pretty BreakToken = "break"
  pretty ContinueToken = "continue"
  pretty IfToken = "if"
  pretty ElseToken = "else"
  pretty EndToken = "end"
  pretty ReturnToken = "return"
  pretty ForToken = "for"
  pretty WhileToken = "while"
  pretty InToken = "in"
  pretty PragmaToken = "pragma"
  pretty (AnnotationKeywordToken str) = str
  pretty InputToken = "input"
  pretty OutputToken = "output"
  pretty ConstToken = "const"
  pretty ReadonlyToken = "readonly"
  pretty MutableToken = "mutable"
  pretty QregToken = "qreg"
  pretty QubitToken = "qubit"
  pretty CregToken = "creg"
  pretty BoolToken = "bool"
  pretty BitToken = "bit"
  pretty IntToken = "int"
  pretty UintToken = "uint"
  pretty FloatToken = "float"
  pretty AngleToken = "angle"
  pretty ComplexToken = "complex"
  pretty ArrayToken = "array"
  pretty VoidToken = "void"
  pretty DurationToken = "duration"
  pretty StretchToken = "stretch"
  pretty GphaseToken = "gphase"
  pretty InvToken = "inv"
  pretty PowToken = "pow"
  pretty CtrlToken = "ctrl"
  pretty NegctrlToken = "negctrl"
  pretty DimToken = "#dim"
  pretty DurationofToken = "durationof"
  pretty DelayToken = "delay"
  pretty ResetToken = "reset"
  pretty MeasureToken = "measure"
  pretty BarrierToken = "barrier"
  pretty (BooleanLiteralToken str) = str
  pretty LbracketToken = "["
  pretty RbracketToken = "]"
  pretty LbraceToken = "{"
  pretty RbraceToken = "}"
  pretty LparenToken = "("
  pretty RparenToken = ")"
  pretty ColonToken = ":"
  pretty SemicolonToken = ";"
  pretty DotToken = "."
  pretty CommaToken = ","
  pretty EqualsToken = "="
  pretty ArrowToken = "->"
  pretty PlusToken = "+"
  pretty DoublePlusToken = "++"
  pretty MinusToken = "-"
  pretty AsteriskToken = "*"
  pretty DoubleAsteriskToken = "**"
  pretty SlashToken = "/"
  pretty PercentToken = "%"
  pretty PipeToken = "|"
  pretty DoublePipeToken = "||"
  pretty AmpersandToken = "&"
  pretty DoubleAmpersandToken = "&&"
  pretty CaretToken = "^"
  pretty AtToken = "@"
  pretty TildeToken = "~"
  pretty ExclamationPointToken = "!"
  pretty (EqualityOperatorToken str) = str
  pretty (CompoundAssignmentOperatorToken str) = str
  pretty (ComparisonOperatorToken str) = str
  pretty (BitshiftOperatorToken str) = str
  pretty (ImaginaryLiteralToken str) = str
  pretty (BinaryIntegerLiteralToken str) = str
  pretty (OctalIntegerLiteralToken str) = str
  pretty (DecimalIntegerLiteralToken str) = str
  pretty (HexIntegerLiteralToken str) = str
  pretty (IdentifierToken str) = str
  pretty (HardwareQubitToken str) = str
  pretty (FloatLiteralToken str) = str
  pretty (TimingLiteralToken str) = str
  pretty (BitstringLiteralToken str) = str
  pretty (WhitespaceToken str) = str
  pretty NewlineToken = "\n"
  pretty (LineCommentToken str) = str
  pretty (BlockCommentToken str) = str
  pretty (VersionSpecifierToken str) = str
  pretty (StringLiteralToken str) = str
  pretty (RemainingLineContentToken str) = str

data Lexeme = Lexeme (Maybe SourceRef) Token
  deriving (Eq, Read, Show)

instance AstNode Lexeme where
  sourceRef (Lexeme ref _) = ref
  pretty (Lexeme _ tok) = pretty tok

token :: Lexeme -> Token
token (Lexeme _ tok) = tok

data ProgramNode = Program Lexeme Lexeme [StatementNode]
  deriving (Eq, Read, Show)

instance AstNode ProgramNode where
  pretty (Program _ ver stmts) = "OPENQASM " ++ pretty ver ++ ";\n\n" ++ concatMap pretty stmts
  sourceRef (Program lex _ _) = sourceRef lex

data StatementNode
  = Pragma Lexeme Lexeme
  | Annotated [AnnotationNode] StatementContentNode
  deriving (Eq, Read, Show)

instance AstNode StatementNode where
  pretty (Pragma _ content) = "pragma " ++ pretty content ++ "\n"
  pretty (Annotated annotations stmt) = concatMap ((++ "\n") . pretty) annotations ++ pretty stmt ++ "\n"

  sourceRef (Pragma lex _) = sourceRef lex
  sourceRef (Annotated annotations stmt) = mplus (msum (map sourceRef annotations)) (sourceRef stmt)

data AnnotationNode = Annotation Lexeme Lexeme
  deriving (Eq, Read, Show)

instance AstNode AnnotationNode where
  pretty (Annotation annotation content) = pretty annotation ++ ' ' : pretty content
  sourceRef (Annotation annotation _) = sourceRef annotation

-- ScopeNode elided, use [StatementNode].

data StatementOrScopeNode = Statement StatementNode | Scope [StatementNode]
  deriving (Eq, Read, Show)

instance AstNode StatementOrScopeNode where
  pretty (Statement stmt) = pretty stmt
  pretty (Scope stmts) = "{\n" ++ indent (concatMap (\s -> pretty s ++ "\n") stmts) ++ "}\n"

  sourceRef (Statement stmt) = sourceRef stmt
  sourceRef (Scope stmts) = msum (map sourceRef stmts)

-- Start top-level statement definitions.

data StatementContentNode
  = -- LET Identifier (EQUALS) aliasExpression (SEMICOLON)
    AliasDeclaration Lexeme Lexeme AliasExpressionNode
  | -- indexedIdentifier assignmentOperator measureExpression (SEMICOLON)
    Assignment IndexedIdentifierNode Lexeme MeasureExpressionNode
  | -- BARRIER gateOperandList? (SEMICOLON)
    Barrier Lexeme [GateOperandNode]
  | -- BOX designator? scope
    Box Lexeme (Maybe ExpressionNode) [StatementNode]
  | -- BREAK (SEMICOLON)
    Break Lexeme
  | -- CAL (LBRACE) CalibrationBlock? (RBRACE)
    Cal Lexeme Lexeme
  | -- DEFCALGRAMMAR StringLiteral
    CalibrationGrammar Lexeme Lexeme
  | -- scalarOrArrayType Identifier ((EQUALS) declarationExpression)? (SEMICOLON)
    ClassicalDeclaration ScalarOrArrayTypeNode Lexeme (Maybe DeclarationExpressionNode)
  | -- CONST scalarType Identifier (EQUALS) declarationExpression (SEMICOLON)
    ConstDeclaration Lexeme ScalarTypeNode Lexeme DeclarationExpressionNode
  | -- CONTINUE (SEMICOLON)
    Continue Lexeme
  | -- DEF Identifier (LPAREN) argumentDefinitionList (RPAREN) returnSignature? scope
    Def Lexeme Lexeme [ArgumentDefinitionNode] (Maybe ScalarTypeNode) [StatementNode]
  | -- DEFCAL defcalTarget ((LPAREN) defcalArgumentDefinitionList (RPAREN))? defcalOperandList returnSignature? (LBRACE) CalibrationBlock? (RBRACE)
    Defcal Lexeme DefcalTargetNode [DefcalArgumentDefinitionNode] [DefcalOperandNode] (Maybe ScalarTypeNode) Lexeme
  | -- DELAY designator gateOperandList? (SEMICOLON)
    Delay Lexeme ExpressionNode [GateOperandNode]
  | -- END (SEMICOLON)
    End Lexeme
  | -- expression (SEMICOLON)
    Expression ExpressionNode
  | -- EXTERN Identifier (LPAREN) externArgumentList (RPAREN) returnSignature? (SEMICOLON)
    Extern Lexeme Lexeme [ExternArgumentNode] (Maybe ScalarTypeNode)
  | -- FOR scalarType Identifier (IN) expression statementOrScope
    For Lexeme ScalarTypeNode Lexeme ExpressionNode StatementOrScopeNode
  | -- FOR scalarType Identifier (IN) (LBRACKET) rangeExpression (RBRACKET) statementOrScope
    RangeFor Lexeme ScalarTypeNode Lexeme RangeExpressionNode StatementOrScopeNode
  | -- FOR scalarType Identifier (IN) setExpression statementOrScope
    SetFor Lexeme ScalarTypeNode Lexeme SetExpressionNode StatementOrScopeNode
  | -- GATE Identifier ((LPAREN) identifierList? (RPAREN))? identifierList scope
    Gate Lexeme Lexeme [Lexeme] [Lexeme] [StatementNode]
  | -- gateModifierList Identifier ((LPAREN) expressionList (RPAREN))? designator? gateOperandList? (SEMICOLON)
    GateCall [GateModifierNode] Lexeme [ExpressionNode] (Maybe ExpressionNode) [GateOperandNode]
  | -- IF (LPAREN) expression (RPAREN) statementOrScope ((ELSE) statementOrScope)?
    If Lexeme ExpressionNode StatementOrScopeNode (Maybe StatementOrScopeNode)
  | -- INCLUDE StringLiteral (SEMICOLON)
    Include Lexeme Lexeme
  | -- INPUT scalarOrArrayType Identifier (SEMICOLON)
    InputIoDeclaration Lexeme ScalarOrArrayTypeNode Lexeme
  | -- OUTPUT scalarTypeOrArrayType Identifier (SEMICOLON)
    OutputIoDeclaration Lexeme ScalarOrArrayTypeNode Lexeme
  | -- MEASURE gateOperand ((ARROW) indexedIdentifier)? (SEMICOLON)
    MeasureArrowAssignment Lexeme GateOperandNode (Maybe IndexedIdentifierNode)
  | -- CREG Identifier designator? (SEMICOLON)
    CregOldStyleDeclaration Lexeme Lexeme (Maybe ExpressionNode)
  | -- QREG Identifier designator? (SEMICOLON)
    QregOldStyleDeclaration Lexeme Lexeme (Maybe ExpressionNode)
  | -- qubitType Identifier (SEMICOLON)
    QuantumDeclaration QubitTypeNode Lexeme
  | -- RESET gateOperand (SEMICOLON)
    Reset Lexeme GateOperandNode
  | -- RETURN measureExpression? (SEMICOLON)
    Return Lexeme (Maybe MeasureExpressionNode)
  | -- WHILE (LPAREN) expression (RPAREN) statementOrScope
    While Lexeme ExpressionNode StatementOrScopeNode
  deriving (Eq, Read, Show)

instance AstNode StatementContentNode where
  pretty (AliasDeclaration _ ident alias) = "let " ++ pretty ident ++ " = " ++ pretty alias ++ ";"
  pretty (Assignment indexedId op expr) = pretty indexedId ++ " " ++ pretty op ++ " " ++ pretty expr ++ ";"
  pretty (Barrier _ gateOperands) = "barrier " ++ prettyList gateOperands ++ ";"
  pretty (Box _ maybeDsgn stmts) = "box" ++ prettyMaybeDsgn maybeDsgn ++ " " ++ prettyBlock stmts
  pretty (Break token) = pretty token ++ ";"
  pretty (Cal _ calBlock) = pretty calBlock
  pretty (CalibrationGrammar _ strLit) = "defcalgrammar " ++ pretty strLit ++ ";"
  pretty (ClassicalDeclaration anyType ident declExpr) =
    pretty anyType ++ " " ++ pretty ident ++ maybe "" ((" = " ++) . pretty) declExpr ++ ";"
  pretty (ConstDeclaration _ scalarType ident declExpr) =
    pretty scalarType ++ " " ++ pretty ident ++ " = " ++ pretty declExpr ++ ";"
  pretty (Continue _) = "continue;"
  pretty (Def _ ident argDefs returnType stmts) =
    "def "
      ++ pretty ident
      ++ "("
      ++ prettyList argDefs
      ++ ") "
      ++ prettyReturnType returnType
      ++ prettyBlock stmts
  pretty (Defcal _ defcalTarget defcalArgs defcalOperands returnType calBlock) =
    "defcal "
      ++ pretty defcalTarget
      ++ (if not $ null defcalArgs then "(" ++ prettyList defcalArgs ++ ") " else " ")
      ++ (if not $ null defcalOperands then prettyList defcalOperands ++ " " else "")
      ++ prettyReturnType returnType
      ++ " "
      ++ pretty calBlock
  pretty (Delay _ designator gateOperands) = "delay" ++ pretty designator ++ " " ++ prettyList gateOperands ++ ";"
  pretty (End _) = "end;"
  pretty (Expression expr) = pretty expr ++ ";"
  pretty (Extern _ ident argDefs returnType) =
    "extern " ++ pretty ident ++ "(" ++ prettyList argDefs ++ ")" ++ prettyReturnType returnType ++ ";"
  pretty (For _ scalarType ident expr loopStmt) =
    "for " ++ pretty scalarType ++ " " ++ pretty ident ++ " in " ++ pretty expr ++ " " ++ pretty loopStmt
  pretty (RangeFor _ scalarType ident rangeExpr loopStmt) =
    "for " ++ pretty scalarType ++ " " ++ pretty ident ++ " in " ++ pretty rangeExpr ++ " " ++ pretty loopStmt
  pretty (SetFor _ scalarType ident setExpr loopStmt) =
    "for " ++ pretty scalarType ++ " " ++ pretty ident ++ " in " ++ pretty setExpr ++ " " ++ pretty loopStmt
  -- gateModifierList Identifier ((LPAREN) expressionList (RPAREN))? designator? gateOperandList? (SEMICOLON)
  pretty (Gate _ ident paramIds regIds stmts) =
    pretty ident
      ++ (if null paramIds then "" else "(" ++ prettyList paramIds ++ ")")
      ++ (" " ++ if null regIds then "" else prettyList regIds)
      ++ prettyBlock stmts
  pretty (GateCall modifiers ident exprs maybeDsgn gateOperands) =
    concatMap ((++ " ") . pretty) modifiers
      ++ pretty ident
      ++ (if null exprs then "" else " (" ++ prettyList exprs ++ ")")
      ++ maybe "" ((" " ++) . pretty) maybeDsgn
      ++ (if null gateOperands then "" else " " ++ prettyList gateOperands)
      ++ ";"
  pretty (If _ testExpr thenStmt (Just elseStmt)) =
    "if (" ++ pretty testExpr ++ ") " ++ pretty thenStmt ++ " else " ++ pretty elseStmt
  pretty (If _ testExpr thenStmt Nothing) = "if (" ++ pretty testExpr ++ ") " ++ pretty thenStmt
  pretty (Include _ strLit) = "include " ++ pretty strLit ++ ";"
  pretty (InputIoDeclaration _ anyType ident) = "input " ++ pretty anyType ++ " " ++ pretty ident ++ ";"
  pretty (OutputIoDeclaration _ anyType ident) = "output " ++ pretty anyType ++ " " ++ pretty ident ++ ";"
  pretty (MeasureArrowAssignment _ gateOperand maybeIndexedId) =
    "measure " ++ pretty gateOperand ++ maybe "" ((" -> " ++) . pretty) maybeIndexedId ++ ";"
  pretty (CregOldStyleDeclaration _ ident maybeDsgn) = "creg " ++ pretty ident ++ maybe "" pretty maybeDsgn ++ ";"
  pretty (QregOldStyleDeclaration _ ident maybeDsgn) = "qreg " ++ pretty ident ++ maybe "" pretty maybeDsgn ++ ";"
  pretty (QuantumDeclaration qubitType ident) = pretty qubitType ++ " " ++ pretty ident ++ ";"
  pretty (Reset _ gateOperand) = "reset " ++ pretty gateOperand ++ ";"
  pretty (Return _ expr) = "return" ++ maybe "" ((" " ++) . pretty) expr ++ ";"
  pretty (While _ condExpr loopStmt) = "while (" ++ pretty condExpr ++ ") " ++ pretty loopStmt

  -- AliasDeclaration Lexeme Lexeme AliasExpressionNode
  sourceRef (AliasDeclaration lex _ _) = sourceRef lex
  -- Assignment IndexedIdentifierNode Lexeme MeasureExpressionNode
  sourceRef (Assignment indexedId _ _) = sourceRef indexedId
  -- Barrier Lexeme [GateOperandNode]
  sourceRef (Barrier lex _) = sourceRef lex
  -- Box Lexeme (Maybe DesignatorNode) [StatementNode]
  sourceRef (Box lex _ _) = sourceRef lex
  -- Break Lexeme
  sourceRef (Break lex) = sourceRef lex
  -- Cal Lexeme Lexeme
  sourceRef (Cal lex _) = sourceRef lex
  -- CalibrationGrammar Lexeme Lexeme
  sourceRef (CalibrationGrammar lex _) = sourceRef lex
  -- ClassicalDeclaration ScalarOrArrayTypeNode Lexeme (Maybe DeclarationExpressionNode)
  sourceRef (ClassicalDeclaration scalarOrArrayType _ _) = sourceRef scalarOrArrayType
  -- ConstDeclaration Lexeme ScalarTypeNode Lexeme DeclarationExpressionNode
  sourceRef (ConstDeclaration lex _ _ _) = sourceRef lex
  -- Continue Lexeme
  sourceRef (Continue lex) = sourceRef lex
  -- Def Lexeme Lexeme [ArgumentDefinitionNode] (Maybe ScalarTypeNode) [StatementNode]
  sourceRef (Def lex _ _ _ _) = sourceRef lex
  -- Defcal Lexeme DefcalTargetNode [DefcalArgumentDefinitionNode] [DefcalOperandNode] (Maybe ScalarTypeNode) (Maybe CalibrationBlockNode)
  sourceRef (Defcal lex _ _ _ _ _) = sourceRef lex
  -- Delay Lexeme DesignatorNode [GateOperandNode]
  sourceRef (Delay lex _ _) = sourceRef lex
  -- End Lexeme
  sourceRef (End lex) = sourceRef lex
  -- Expression ExpressionNode
  sourceRef (Expression expr) = sourceRef expr
  -- Extern Lexeme Lexeme [ArgumentDefinitionNode] (Maybe ScalarTypeNode)
  sourceRef (Extern lex _ _ _) = sourceRef lex
  -- For Lexeme ScalarTypeNode Lexeme ExpressionNode StatementOrScopeNode
  sourceRef (For lex _ _ _ _) = sourceRef lex
  -- RangeFor Lexeme ScalarTypeNode Lexeme RangeExpressionNode StatementOrScopeNode
  sourceRef (RangeFor lex _ _ _ _) = sourceRef lex
  -- SetFor Lexeme ScalarTypeNode Lexeme SetExpressionNode StatementOrScopeNode
  sourceRef (SetFor lex _ _ _ _) = sourceRef lex
  -- Gate Lexeme [Lexeme] [Lexeme]
  sourceRef (Gate lex _ _ _ _) = sourceRef lex
  -- GateCall [GateModifierNode] Lexeme [ExpressionNode] (Maybe DesignatorNode) [GateOperandNode]
  sourceRef (GateCall gateModifiers lex _ _ _) = mplus (msum (map sourceRef gateModifiers)) (sourceRef lex)
  -- If Lexeme ExpressionNode StatementOrScopeNode (Maybe StatementOrScopeNode)
  sourceRef (If lex _ _ _) = sourceRef lex
  -- Include Lexeme Lexeme
  sourceRef (Include lex _) = sourceRef lex
  -- InputIoDeclaration Lexeme ScalarOrArrayTypeNode Lexeme
  sourceRef (InputIoDeclaration lex _ _) = sourceRef lex
  -- OutputIoDeclaration Lexeme ScalarOrArrayTypeNode Lexeme
  sourceRef (OutputIoDeclaration lex _ _) = sourceRef lex
  -- MeasureArrowAssignment Lexeme GateOperandNode (Maybe IndexedIdentifierNode)
  sourceRef (MeasureArrowAssignment lex _ _) = sourceRef lex
  -- CregOldStyleDeclaration Lexeme Lexeme (Maybe DesignatorNode)
  sourceRef (CregOldStyleDeclaration ident _ _) = sourceRef ident
  -- QregOldStyleDeclaration Lexeme Lexeme (Maybe DesignatorNode)
  sourceRef (QregOldStyleDeclaration ident _ _) = sourceRef ident
  -- QuantumDeclaration QubitTypeNode Lexeme
  sourceRef (QuantumDeclaration qubitType _) = sourceRef qubitType
  -- Reset Lexeme GateOperandNode
  sourceRef (Reset lex _) = sourceRef lex
  -- Return Lexeme (Maybe MeasureExpressionNode)
  sourceRef (Return lex _) = sourceRef lex
  -- While Lexeme ExpressionNode StatementOrScopeNode
  sourceRef (While lex _ _) = sourceRef lex

data ScalarOrArrayTypeNode = Scalar ScalarTypeNode | Array ArrayTypeNode
  deriving (Eq, Read, Show)

instance AstNode ScalarOrArrayTypeNode where
  pretty (Scalar scalarType) = pretty scalarType
  pretty (Array arrayType) = pretty arrayType

  sourceRef (Scalar scalarType) = sourceRef scalarType
  sourceRef (Array arrayType) = sourceRef arrayType

-- Start expression definitions.

data ExpressionNode
  = ParenExpression ExpressionNode
  | IndexExpression ExpressionNode IndexOperatorNode
  | UnaryOperatorExpression Lexeme ExpressionNode
  | BinaryOperatorExpression ExpressionNode Lexeme ExpressionNode
  | CastExpression ScalarOrArrayTypeNode ExpressionNode
  | DurationOfExpression Lexeme [StatementNode]
  | CallExpression Lexeme [ExpressionNode]
  | Identifier Lexeme
  | IntegerLiteral Lexeme
  | FloatLiteral Lexeme
  | ImaginaryLiteral Lexeme
  | BooleanLiteral Lexeme
  | BitstringLiteral Lexeme
  | TimingLiteral Lexeme
  | HardwareQubitLiteral Lexeme
  deriving (Eq, Read, Show)

instance AstNode ExpressionNode where
  pretty (ParenExpression expr) = "(" ++ pretty expr ++ ")"
  pretty (IndexExpression expr index) = "(" ++ pretty expr ++ ")" ++ pretty index
  pretty (UnaryOperatorExpression op expr) = pretty op ++ "(" ++ pretty expr ++ ")"
  pretty (BinaryOperatorExpression exprA op exprB) =
    "(" ++ pretty exprA ++ ") " ++ pretty op ++ " (" ++ pretty exprB ++ ")"
  pretty (CastExpression anyType expr) = pretty anyType ++ "(" ++ pretty expr ++ ")"
  pretty (DurationOfExpression _ stmts) = "durationof(" ++ prettyBlock stmts ++ ")"
  pretty (CallExpression ident exprs) = pretty ident ++ "(" ++ prettyList exprs ++ ")"
  pretty (Identifier ident) = pretty ident
  pretty (IntegerLiteral intLit) = pretty intLit
  pretty (FloatLiteral floatLit) = pretty floatLit
  pretty (ImaginaryLiteral imagLit) = pretty imagLit
  pretty (BooleanLiteral boolLit) = pretty boolLit
  pretty (BitstringLiteral bitsLit) = pretty bitsLit
  pretty (TimingLiteral timingLit) = pretty timingLit
  pretty (HardwareQubitLiteral hwQubit) = pretty hwQubit

  sourceRef (ParenExpression expr) = sourceRef expr
  sourceRef (IndexExpression expr index) = msum [sourceRef expr, sourceRef index]
  sourceRef (UnaryOperatorExpression op expr) = msum [sourceRef op, sourceRef expr]
  sourceRef (BinaryOperatorExpression exprA op exprB) = msum [sourceRef exprA, sourceRef op, sourceRef exprB]
  sourceRef (CastExpression anyType expr) = msum [sourceRef anyType, sourceRef expr]
  sourceRef (DurationOfExpression lex _) = sourceRef lex
  sourceRef (CallExpression ident exprs) = msum (sourceRef ident : map sourceRef exprs)
  sourceRef (Identifier ident) = sourceRef ident
  sourceRef (IntegerLiteral intLit) = sourceRef intLit
  sourceRef (FloatLiteral floatLit) = sourceRef floatLit
  sourceRef (ImaginaryLiteral imagLit) = sourceRef imagLit
  sourceRef (BooleanLiteral boolLit) = sourceRef boolLit
  sourceRef (BitstringLiteral bitsLit) = sourceRef bitsLit
  sourceRef (TimingLiteral timingLit) = sourceRef timingLit
  sourceRef (HardwareQubitLiteral hwQubit) = sourceRef hwQubit

-- Special-case expressions that are only valid in certain contexts.  These are
-- not in the expression tree, but can contain elements that are within it.
newtype AliasExpressionNode = AliasExpression [ExpressionNode]
  deriving (Eq, Read, Show)

instance AstNode AliasExpressionNode where
  pretty (AliasExpression exprs) = intercalate " ++ " $ map pretty exprs
  sourceRef (AliasExpression exprs) = sourceRef (head exprs)

data DeclarationExpressionNode
  = ArrayLiteralDeclarationExpression ArrayLiteralNode
  | ExpressionDeclarationExpression MeasureExpressionNode
  deriving (Eq, Read, Show)

instance AstNode DeclarationExpressionNode where
  pretty (ArrayLiteralDeclarationExpression arrayLit) = "= " ++ pretty arrayLit
  pretty (ExpressionDeclarationExpression expr) = "= " ++ pretty expr

  sourceRef (ArrayLiteralDeclarationExpression arrayLit) = sourceRef arrayLit
  sourceRef (ExpressionDeclarationExpression expr) = sourceRef expr

data MeasureExpressionNode
  = PlainExpression ExpressionNode
  | MeasureExpression Lexeme GateOperandNode
  deriving (Eq, Read, Show)

instance AstNode MeasureExpressionNode where
  pretty (PlainExpression expr) = pretty expr
  pretty (MeasureExpression _ gateOperand) = "measure " ++ pretty gateOperand

  sourceRef (PlainExpression expr) = sourceRef expr
  sourceRef (MeasureExpression lex _) = sourceRef lex

data RangeOrExpressionIndexNode
  = ExpressionIndex ExpressionNode
  | RangeIndex RangeExpressionNode
  deriving (Eq, Read, Show)

instance AstNode RangeOrExpressionIndexNode where
  pretty (ExpressionIndex expr) = pretty expr
  pretty (RangeIndex rangeExpr) = pretty rangeExpr

  sourceRef (ExpressionIndex expr) = sourceRef expr
  sourceRef (RangeIndex rangeExpr) = sourceRef rangeExpr

data RangeExpressionNode
  = RangeExpression (Maybe SourceRef) (Maybe ExpressionNode) (Maybe ExpressionNode) (Maybe ExpressionNode)
  deriving (Eq, Read, Show)

instance AstNode RangeExpressionNode where
  pretty (RangeExpression _ start end stride) =
    maybe "" pretty start ++ ":" ++ maybe "" pretty end ++ maybe "" ((" : " ++) . pretty) stride
  sourceRef (RangeExpression ref _ _ _) = ref

newtype SetExpressionNode = SetExpression [ExpressionNode]
  deriving (Eq, Read, Show)

instance AstNode SetExpressionNode where
  pretty (SetExpression exprs) = "{" ++ prettyList exprs ++ "}"
  sourceRef (SetExpression exprs) = msum $ map sourceRef exprs

newtype ArrayLiteralNode = ArrayLiteral [ArrayLiteralElementNode]
  deriving (Eq, Read, Show)

instance AstNode ArrayLiteralNode where
  pretty (ArrayLiteral elements) = "{" ++ prettyList elements ++ "}"
  sourceRef (ArrayLiteral elements) = msum $ map sourceRef elements

data ArrayLiteralElementNode
  = ExpressionArrayElement ExpressionNode
  | ArrayArrayElement ArrayLiteralNode
  deriving (Eq, Read, Show)

instance AstNode ArrayLiteralElementNode where
  pretty (ExpressionArrayElement expr) = pretty expr
  pretty (ArrayArrayElement arrayLit) = pretty arrayLit

  sourceRef (ExpressionArrayElement expr) = sourceRef expr
  sourceRef (ArrayArrayElement arrayLit) = sourceRef arrayLit

data IndexOperatorNode
  = SetIndex SetExpressionNode
  | IndexList [RangeOrExpressionIndexNode]
  deriving (Eq, Read, Show)

instance AstNode IndexOperatorNode where
  pretty (SetIndex setExpr) = "[" ++ pretty setExpr ++ "]"
  pretty (IndexList exprs) = "[" ++ prettyList exprs ++ "]"

  sourceRef (SetIndex setExpr) = sourceRef setExpr
  sourceRef (IndexList exprs) = msum $ map sourceRef exprs

data IndexedIdentifierNode = IndexedIdentifier Lexeme [IndexOperatorNode]
  deriving (Eq, Read, Show)

instance AstNode IndexedIdentifierNode where
  pretty (IndexedIdentifier ident indices) = pretty ident ++ concatMap pretty indices
  sourceRef (IndexedIdentifier ident indices) = msum (sourceRef ident : map sourceRef indices)

-- Start type definitions.

-- ReturnSignatureNode elided, use ScalarTypeNode.

data GateModifierNode
  = InvGateModifier Lexeme
  | PowGateModifier Lexeme ExpressionNode
  | CtrlGateModifier Lexeme (Maybe ExpressionNode)
  | NegCtrlGateModifier Lexeme (Maybe ExpressionNode)
  deriving (Eq, Read, Show)

instance AstNode GateModifierNode where
  pretty (InvGateModifier _) = "inv at"
  pretty (PowGateModifier _ expr) = "pow(" ++ pretty expr ++ ")"
  pretty (CtrlGateModifier _ maybeExpr) = "ctrl " ++ maybe "" (\e -> "(" ++ pretty e ++ ") ") maybeExpr ++ "at"
  pretty (NegCtrlGateModifier _ maybeExpr) = "negctrl " ++ maybe "" (\e -> "(" ++ pretty e ++ ") ") maybeExpr ++ "at"

  sourceRef (InvGateModifier lex) = sourceRef lex
  sourceRef (PowGateModifier lex _) = sourceRef lex
  sourceRef (CtrlGateModifier lex _) = sourceRef lex
  sourceRef (NegCtrlGateModifier lex _) = sourceRef lex

data ScalarTypeNode
  = BitType Lexeme (Maybe ExpressionNode)
  | IntType Lexeme (Maybe ExpressionNode)
  | UintType Lexeme (Maybe ExpressionNode)
  | FloatType Lexeme (Maybe ExpressionNode)
  | AngleType Lexeme (Maybe ExpressionNode)
  | BoolType Lexeme
  | DurationType Lexeme
  | StretchType Lexeme
  | ComplexType Lexeme (Maybe ScalarTypeNode)
  deriving (Eq, Read, Show)

instance AstNode ScalarTypeNode where
  pretty (BitType _ maybeDsgn) = "bit" ++ prettyMaybeDsgn maybeDsgn
  pretty (IntType _ maybeDsgn) = "int" ++ prettyMaybeDsgn maybeDsgn
  pretty (UintType _ maybeDsgn) = "uint" ++ prettyMaybeDsgn maybeDsgn
  pretty (FloatType _ maybeDsgn) = "float" ++ prettyMaybeDsgn maybeDsgn
  pretty (AngleType _ maybeDsgn) = "angle" ++ prettyMaybeDsgn maybeDsgn
  pretty (BoolType _) = "bool"
  pretty (DurationType _) = "duration"
  pretty (StretchType _) = "stretch"
  pretty (ComplexType _ maybeScalarType) = "complex" ++ maybe "" (\sclr -> "[" ++ pretty sclr ++ "]") maybeScalarType

  sourceRef (BitType lex _) = sourceRef lex
  sourceRef (IntType lex _) = sourceRef lex
  sourceRef (UintType lex _) = sourceRef lex
  sourceRef (FloatType lex _) = sourceRef lex
  sourceRef (AngleType lex _) = sourceRef lex
  sourceRef (BoolType lex) = sourceRef lex
  sourceRef (DurationType lex) = sourceRef lex
  sourceRef (StretchType lex) = sourceRef lex
  sourceRef (ComplexType lex _) = sourceRef lex

data QubitTypeNode = QubitType Lexeme (Maybe ExpressionNode)
  deriving (Eq, Read, Show)

instance AstNode QubitTypeNode where
  pretty (QubitType _ maybeDsgn) = "qubit" ++ prettyMaybeDsgn maybeDsgn
  sourceRef (QubitType lex _) = sourceRef lex

data ArrayTypeNode = ArrayType Lexeme ScalarTypeNode [ExpressionNode]
  deriving (Eq, Read, Show)

instance AstNode ArrayTypeNode where
  pretty (ArrayType lex scalarType exprs) = "array[" ++ pretty scalarType ++ ", " ++ prettyList exprs ++ "]"
  sourceRef (ArrayType lex _ _) = sourceRef lex

data ArrayReferenceTypeNode
  = ReadonlyArrayReferenceType Lexeme ScalarTypeNode [ExpressionNode]
  | MutableArrayReferenceType Lexeme ScalarTypeNode [ExpressionNode]
  | ReadonlyArrayReferenceDimType Lexeme ScalarTypeNode ExpressionNode
  | MutableArrayReferenceDimType Lexeme ScalarTypeNode ExpressionNode
  deriving (Eq, Read, Show)

instance AstNode ArrayReferenceTypeNode where
  pretty (ReadonlyArrayReferenceType _ scalarType exprs) =
    "readonly array[" ++ pretty scalarType ++ ", " ++ prettyList exprs ++ "]"
  pretty (MutableArrayReferenceType _ scalarType exprs) =
    "mutable array[" ++ pretty scalarType ++ ", " ++ prettyList exprs ++ "]"
  pretty (ReadonlyArrayReferenceDimType _ scalarType expr) =
    "readonly array[" ++ pretty scalarType ++ ", #dim = " ++ pretty expr ++ "]"
  pretty (MutableArrayReferenceDimType _ scalarType expr) =
    "mutable array[" ++ pretty scalarType ++ ", #dim = " ++ pretty expr ++ "]"

  sourceRef (ReadonlyArrayReferenceType lex _ _) = sourceRef lex
  sourceRef (MutableArrayReferenceType lex _ _) = sourceRef lex
  sourceRef (ReadonlyArrayReferenceDimType lex _ _) = sourceRef lex
  sourceRef (MutableArrayReferenceDimType lex _ _) = sourceRef lex

-- Start miscellany.

-- DesignatorNode elided, use ExpressionNode.

data DefcalTargetNode
  = MeasureDefcalTarget Lexeme
  | ResetDefcalTarget Lexeme
  | DelayDefcalTarget Lexeme
  | IdentifierDefcalTarget Lexeme
  deriving (Eq, Read, Show)

instance AstNode DefcalTargetNode where
  pretty (MeasureDefcalTarget lex) = "measure"
  pretty (ResetDefcalTarget lex) = "reset"
  pretty (DelayDefcalTarget lex) = "delay"
  pretty (IdentifierDefcalTarget ident) = pretty ident

  sourceRef (MeasureDefcalTarget lex) = sourceRef lex
  sourceRef (ResetDefcalTarget lex) = sourceRef lex
  sourceRef (DelayDefcalTarget lex) = sourceRef lex
  sourceRef (IdentifierDefcalTarget ident) = sourceRef ident

data DefcalArgumentDefinitionNode
  = ExpressionDefcalArgument ExpressionNode
  | ArgumentDefinitionDefcalArgument ArgumentDefinitionNode
  deriving (Eq, Read, Show)

instance AstNode DefcalArgumentDefinitionNode where
  pretty (ExpressionDefcalArgument expr) = pretty expr
  pretty (ArgumentDefinitionDefcalArgument argDef) = pretty argDef

  sourceRef (ExpressionDefcalArgument expr) = sourceRef expr
  sourceRef (ArgumentDefinitionDefcalArgument argDef) = sourceRef argDef

data DefcalOperandNode
  = IdentifierDefcal Lexeme
  | HardwareQubitDefcal Lexeme
  deriving (Eq, Read, Show)

instance AstNode DefcalOperandNode where
  pretty (IdentifierDefcal ident) = pretty ident
  pretty (HardwareQubitDefcal hwQubit) = pretty hwQubit

  sourceRef (IdentifierDefcal ident) = sourceRef ident
  sourceRef (HardwareQubitDefcal hwQubit) = sourceRef hwQubit

data GateOperandNode
  = IdentifierGateOperand IndexedIdentifierNode
  | HardwareQubitGateOperand Lexeme
  deriving (Eq, Read, Show)

instance AstNode GateOperandNode where
  pretty (IdentifierGateOperand indexedId) = pretty indexedId
  pretty (HardwareQubitGateOperand hwQubit) = pretty hwQubit

  sourceRef (IdentifierGateOperand indexedId) = sourceRef indexedId
  sourceRef (HardwareQubitGateOperand hwQubit) = sourceRef hwQubit

data ExternArgumentNode
  = ScalarExternArgument ScalarTypeNode
  | ArrayExternArgument ArrayReferenceTypeNode
  | CregExternArgument Lexeme (Maybe ExpressionNode)
  deriving (Eq, Read, Show)

instance AstNode ExternArgumentNode where
  pretty (ScalarExternArgument scalarType) = pretty scalarType
  pretty (ArrayExternArgument arrayRefType) = pretty arrayRefType
  pretty (CregExternArgument _ maybeDsgn) = "creg" ++ prettyMaybeDsgn maybeDsgn

  sourceRef (ScalarExternArgument scalarType) = sourceRef scalarType
  sourceRef (ArrayExternArgument arrayRefType) = sourceRef arrayRefType
  sourceRef (CregExternArgument lex _) = sourceRef lex

data ArgumentDefinitionNode
  = -- scalarType Identifier
    ScalarArgument ScalarTypeNode Lexeme
  | -- qubitType Identifier
    QubitArgument QubitTypeNode Lexeme
  | -- CREG Identifier designator?
    CregArgument Lexeme Lexeme (Maybe ExpressionNode)
  | -- QREG Identifier designator?
    QregArgument Lexeme Lexeme (Maybe ExpressionNode)
  | -- arrayReferenceType Identifier
    ArrayArgument ArrayReferenceTypeNode Lexeme
  deriving (Eq, Read, Show)

instance AstNode ArgumentDefinitionNode where
  pretty :: ArgumentDefinitionNode -> String
  pretty (ScalarArgument scalarType ident) = pretty scalarType ++ " " ++ pretty ident
  pretty (QubitArgument qubitType ident) = pretty qubitType ++ " " ++ pretty ident
  pretty (CregArgument _ ident maybeDsgn) = "creg " ++ pretty ident ++ prettyMaybeDsgn maybeDsgn
  pretty (QregArgument _ ident maybeDsgn) = "qreg " ++ pretty ident ++ prettyMaybeDsgn maybeDsgn
  pretty (ArrayArgument arrayRefType ident) = pretty arrayRefType ++ " " ++ pretty ident

  sourceRef (ScalarArgument scalarType _) = sourceRef scalarType
  sourceRef (QubitArgument qubitType _) = sourceRef qubitType
  sourceRef (CregArgument lex _ _) = sourceRef lex
  sourceRef (QregArgument lex _ _) = sourceRef lex
  sourceRef (ArrayArgument arrayRefType _) = sourceRef arrayRefType

-- Utility functions

prettyBlock :: (Foldable t, AstNode a) => t a -> [Char]
prettyBlock stmts = "{\n" ++ concatMap ((++ "\n") . pretty) stmts ++ "}"

prettyList :: (AstNode a) => [a] -> [Char]
prettyList list = intercalate ", " (map pretty list)

prettyMaybeDsgn :: Maybe ExpressionNode -> String
prettyMaybeDsgn (Just expr) = "[" ++ pretty expr ++ "]"
prettyMaybeDsgn Nothing = ""

prettyReturnType :: (AstNode a) => Maybe a -> String
prettyReturnType (Just returnType) = "-> " ++ pretty returnType
prettyReturnType Nothing = ""

indent :: String -> String
indent block = concatMap (\s -> "  " ++ s ++ "\n") $ lines block
