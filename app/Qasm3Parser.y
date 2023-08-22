{
module Qasm3Parser (parseQasm3, parseQasm3String) where

import Ast
import Data.Char
import Qasm3
import Qasm3Lexer qualified as L
}

%name parseQasm3 program

%tokentype { L.Lexeme }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { Lexeme _ EofToken }

%token
    OPENQASM                                { Lexeme _ OpenqasmToken }
    INCLUDE                                 { Lexeme _ IncludeToken }
    DEFCALGRAMMAR                           { Lexeme _ DefcalgrammarToken }
    DEF                                     { Lexeme _ DefToken }
    CAL                                     { Lexeme _ CalToken }
    DEFCAL                                  { Lexeme _ DefcalToken }
    GATE                                    { Lexeme _ GateToken }
    EXTERN                                  { Lexeme _ ExternToken }
    BOX                                     { Lexeme _ BoxToken }
    LET                                     { Lexeme _ LetToken }
    BREAK                                   { Lexeme _ BreakToken }
    CONTINUE                                { Lexeme _ ContinueToken }
    IF                                      { Lexeme _ IfToken }
    ELSE                                    { Lexeme _ ElseToken }
    END                                     { Lexeme _ EndToken }
    RETURN                                  { Lexeme _ ReturnToken }
    FOR                                     { Lexeme _ ForToken }
    WHILE                                   { Lexeme _ WhileToken }
    IN                                      { Lexeme _ InToken }
    PRAGMA                                  { Lexeme _ PragmaToken }
    AnnotationKeyword                       { Lexeme _ (AnnotationKeywordToken _) }
    INPUT                                   { Lexeme _ InputToken }
    OUTPUT                                  { Lexeme _ OutputToken }
    CONST                                   { Lexeme _ ConstToken }
    READONLY                                { Lexeme _ ReadonlyToken }
    MUTABLE                                 { Lexeme _ MutableToken }
    QREG                                    { Lexeme _ QregToken }
    QUBIT                                   { Lexeme _ QubitToken }
    CREG                                    { Lexeme _ CregToken }
    BOOL                                    { Lexeme _ BoolToken }
    BIT                                     { Lexeme _ BitToken }
    INT                                     { Lexeme _ IntToken }
    UINT                                    { Lexeme _ UintToken }
    FLOAT                                   { Lexeme _ FloatToken }
    ANGLE                                   { Lexeme _ AngleToken }
    COMPLEX                                 { Lexeme _ ComplexToken }
    ARRAY                                   { Lexeme _ ArrayToken }
    VOID                                    { Lexeme _ VoidToken }
    DURATION                                { Lexeme _ DurationToken }
    STRETCH                                 { Lexeme _ StretchToken }
    GPHASE                                  { Lexeme _ GphaseToken }
    INV                                     { Lexeme _ InvToken }
    POW                                     { Lexeme _ PowToken }
    CTRL                                    { Lexeme _ CtrlToken }
    NEGCTRL                                 { Lexeme _ NegctrlToken }
    DIM                                     { Lexeme _ DimToken }
    DURATIONOF                              { Lexeme _ DurationofToken }
    DELAY                                   { Lexeme _ DelayToken }
    RESET                                   { Lexeme _ ResetToken }
    MEASURE                                 { Lexeme _ MeasureToken }
    BARRIER                                 { Lexeme _ BarrierToken }
    BooleanLiteral                          { Lexeme _ (BooleanLiteralToken _) }
    LBRACKET                                { Lexeme _ LbracketToken }
    RBRACKET                                { Lexeme _ RbracketToken }
    LBRACE                                  { Lexeme _ LbraceToken }
    RBRACE                                  { Lexeme _ RbraceToken }
    LPAREN                                  { Lexeme _ LparenToken }
    RPAREN                                  { Lexeme _ RparenToken }
    COLON                                   { Lexeme _ ColonToken }
    SEMICOLON                               { Lexeme _ SemicolonToken }
    DOT                                     { Lexeme _ DotToken }
    COMMA                                   { Lexeme _ CommaToken }
    EQUALS                                  { Lexeme _ EqualsToken }
    ARROW                                   { Lexeme _ ArrowToken }
    PLUS                                    { Lexeme _ PlusToken }
    DOUBLE_PLUS                             { Lexeme _ DoublePlusToken }
    MINUS                                   { Lexeme _ MinusToken }
    ASTERISK                                { Lexeme _ AsteriskToken }
    DOUBLE_ASTERISK                         { Lexeme _ DoubleAsteriskToken }
    SLASH                                   { Lexeme _ SlashToken }
    PERCENT                                 { Lexeme _ PercentToken }
    PIPE                                    { Lexeme _ PipeToken }
    DOUBLE_PIPE                             { Lexeme _ DoublePipeToken }
    AMPERSAND                               { Lexeme _ AmpersandToken }
    DOUBLE_AMPERSAND                        { Lexeme _ DoubleAmpersandToken }
    CARET                                   { Lexeme _ CaretToken }
    AT                                      { Lexeme _ AtToken }
    TILDE                                   { Lexeme _ TildeToken }
    EXCLAMATION_POINT                       { Lexeme _ ExclamationPointToken }
    EqualityOperator                        { Lexeme _ (EqualityOperatorToken _) }
    CompoundAssignmentOperator              { Lexeme _ (CompoundAssignmentOperatorToken _) }
    ComparisonOperator                      { Lexeme _ (ComparisonOperatorToken _) }
    BitshiftOperator                        { Lexeme _ (BitshiftOperatorToken _) }
    IMAG                                    { Lexeme _ ImagToken }
    ImaginaryLiteral                        { Lexeme _ (ImaginaryLiteralToken _) }
    BinaryIntegerLiteral                    { Lexeme _ (BinaryIntegerLiteralToken _) }
    OctalIntegerLiteral                     { Lexeme _ (OctalIntegerLiteralToken _) }
    DecimalIntegerLiteral                   { Lexeme _ (DecimalIntegerLiteralToken _) }
    HexIntegerLiteral                       { Lexeme _ (HexIntegerLiteralToken _) }
    Identifier                              { Lexeme _ (IdentifierToken _) }
    HardwareQubit                           { Lexeme _ (HardwareQubitToken _) }
    FloatLiteral                            { Lexeme _ (FloatLiteralToken _) }
    TimingLiteral                           { Lexeme _ (TimingLiteralToken _) }
    BitstringLiteral                        { Lexeme _ (BitstringLiteralToken _) }
    Whitespace                              { Lexeme _ (WhitespaceToken _) }
    Newline                                 { Lexeme _ (NewlineToken _) }
    LineComment                             { Lexeme _ (LineCommentToken _) }
    BlockComment                            { Lexeme _ (BlockCommentToken _) }
    VERSION_IDENTIFER_WHITESPACE            { Lexeme _ VersionIdentiferWhitespaceToken }
    VersionSpecifier                        { Lexeme _ (VersionSpecifierToken _) }
    ARBITRARY_STRING_WHITESPACE             { Lexeme _ ArbitraryStringWhitespaceToken }
    StringLiteral                           { Lexeme _ (StringLiteralToken _) }
    EAT_INITIAL_SPACE                       { Lexeme _ EatInitialSpaceToken }
    EAT_LINE_END                            { Lexeme _ EatLineEndToken }
    RemainingLineContent                    { Lexeme _ (RemainingLineContentToken _) }
    CAL_PRELUDE_WHITESPACE                  { Lexeme _ CalPreludeWhitespaceToken }
    CAL_PRELUDE_COMMENT                     { Lexeme _ CalPreludeCommentToken }
    CAL_PRELUDE_LBRACE                      { Lexeme _ CalPreludeLbraceToken }
    DEFCAL_PRELUDE_WHITESPACE               { Lexeme _ DefcalPreludeWhitespaceToken }
    DEFCAL_PRELUDE_COMMENT                  { Lexeme _ DefcalPreludeCommentToken }
    DEFCAL_PRELUDE_LBRACE                   { Lexeme _ DefcalPreludeLbraceToken }
    DEFCAL_PRELUDE_QREG                     { Lexeme _ DefcalPreludeQregToken }
    DEFCAL_PRELUDE_QUBIT                    { Lexeme _ DefcalPreludeQubitToken }
    DEFCAL_PRELUDE_CREG                     { Lexeme _ DefcalPreludeCregToken }
    DEFCAL_PRELUDE_BOOL                     { Lexeme _ DefcalPreludeBoolToken }
    DEFCAL_PRELUDE_BIT                      { Lexeme _ DefcalPreludeBitToken }
    DEFCAL_PRELUDE_INT                      { Lexeme _ DefcalPreludeIntToken }
    DEFCAL_PRELUDE_UINT                     { Lexeme _ DefcalPreludeUintToken }
    DEFCAL_PRELUDE_ANGLE                    { Lexeme _ DefcalPreludeAngleToken }
    DEFCAL_PRELUDE_FLOAT                    { Lexeme _ DefcalPreludeFloatToken }
    DEFCAL_PRELUDE_COMPLEX                  { Lexeme _ DefcalPreludeComplexToken }
    DEFCAL_PRELUDE_ARRAY                    { Lexeme _ DefcalPreludeArrayToken }
    DEFCAL_PRELUDE_DURATION                 { Lexeme _ DefcalPreludeDurationToken }
    DEFCAL_PRELUDE_LBRACKET                 { Lexeme _ DefcalPreludeLbracketToken }
    DEFCAL_PRELUDE_RBRACKET                 { Lexeme _ DefcalPreludeRbracketToken }
    DEFCAL_PRELUDE_LPAREN                   { Lexeme _ DefcalPreludeLparenToken }
    DEFCAL_PRELUDE_RPAREN                   { Lexeme _ DefcalPreludeRparenToken }
    DEFCAL_PRELUDE_ARROW                    { Lexeme _ DefcalPreludeArrowToken }
    DEFCAL_PRELUDE_COMMA                    { Lexeme _ DefcalPreludeCommaToken }
    DEFCAL_PRELUDE_PLUS                     { Lexeme _ DefcalPreludePlusToken }
    DEFCAL_PRELUDE_MINUS                    { Lexeme _ DefcalPreludeMinusToken }
    DEFCAL_PRELUDE_ASTERISK                 { Lexeme _ DefcalPreludeAsteriskToken }
    DEFCAL_PRELUDE_SLASH                    { Lexeme _ DefcalPreludeSlashToken }
    DEFCAL_PRELUDE_BitshiftOperator         { Lexeme _ (DefcalPreludeBitshiftOperatorToken _) }
    DEFCAL_PRELUDE_BitstringLiteral         { Lexeme _ (DefcalPreludeBitstringLiteralToken _) }
    DEFCAL_PRELUDE_BinaryIntegerLiteral     { Lexeme _ (DefcalPreludeBinaryIntegerLiteralToken _) }
    DEFCAL_PRELUDE_OctalIntegerLiteral      { Lexeme _ (DefcalPreludeOctalIntegerLiteralToken _) }
    DEFCAL_PRELUDE_DecimalIntegerLiteral    { Lexeme _ (DefcalPreludeDecimalIntegerLiteralToken _) }
    DEFCAL_PRELUDE_HexIntegerLiteral        { Lexeme _ (DefcalPreludeHexIntegerLiteralToken _) }
    DEFCAL_PRELUDE_FloatLiteral             { Lexeme _ (DefcalPreludeFloatLiteralToken _) }
    DEFCAL_PRELUDE_MEASURE                  { Lexeme _ DefcalPreludeMeasureToken }
    DEFCAL_PRELUDE_DELAY                    { Lexeme _ DefcalPreludeDelayToken }
    DEFCAL_PRELUDE_RESET                    { Lexeme _ DefcalPreludeResetToken }
    DEFCAL_PRELUDE_Identifier               { Lexeme _ (DefcalPreludeIdentifierToken _) }
    DEFCAL_PRELUDE_HardwareQubit            { Lexeme _ (DefcalPreludeHardwareQubitToken _) }
    CalibrationBlock                        { Lexeme _ (CalibrationBlockToken _) }
    CAL_BLOCK_RBRACE                        { Lexeme _ CalBlockRbraceToken }
 
%%

many_rev(p)
    :                               { [] }
    | many_rev(p) p                 { $2 : $1 }

many(p)
    : many_rev(p)                   { reverse $1 }

program :: { ProgramNode }
    : version many(statement)       { Program $1 $2 }

version :: { VersionSpecifierNode }
    : OPENQASM VersionSpecifier SEMICOLON
                                    { VersionSpecifier $2 }

-- A statement is any valid single statement of an OpenQASM 3 program, with the
-- exception of the version-definition statement (which must be unique, and the
-- first statement of the file if present).  This file just defines rules for
-- parsing; we leave semantic analysis and rejection of invalid scopes for
-- compiler implementations.
statement :: { StatementNode }
    : pragma                        { $1 }
    | many(annotation) statementContent
      { Annotated Nothing $1 $2 }

statementContent :: { StatementContentNode }
    -- All the actual statements of the language.
    -- : aliasDeclarationStatement
    -- | assignmentStatement
    -- | barrierStatement
    -- | boxStatement
    -- | breakStatement
    -- | calStatement
    -- | calibrationGrammarStatement
    -- | classicalDeclarationStatement
    -- | constDeclarationStatement
    -- | continueStatement
    -- | defStatement
    -- | defcalStatement
    -- | delayStatement
    -- | endStatement
    -- | expressionStatement
    -- | externStatement
    -- | forStatement
    -- | gateCallStatement
    -- | gateStatement
    -- | ifStatement
    -- | includeStatement
    -- | ioDeclarationStatement
    -- | measureArrowAssignmentStatement
    -- | oldStyleDeclarationStatement
    -- | quantumDeclarationStatement
    -- | resetStatement
    -- | returnStatement
    -- | whileStatement
    : endStatement                  { End }

annotation :: { AnnotationNode }
    : AnnotationKeyword RemainingLineContent
    {
      let (AnnotationKeywordToken keyword) = token $1
       in let (RemainingLineContentToken content) = token $2
           in Annotation keyword content
    }

scope :: { [StatementNode] }
    : LBRACE many(statement) RBRACE { $2 }

pragma :: { StatementNode }
    : PRAGMA RemainingLineContent
    {
      let (RemainingLineContentToken content) = token $2
       in Pragma Nothing content
    }

statementOrScope :: { StatementOrScopeNode }
    : statement { Statement $1 }
    | scope { Scope $1 }



--------------
endStatement :: { StatementContentNode }
    : END SEMICOLON { End }

{-

{- Start top-level statement definitions. -}

-- Inclusion statements.
calibrationGrammarStatement: DEFCALGRAMMAR StringLiteral SEMICOLON {}
includeStatement: INCLUDE StringLiteral SEMICOLON {}

-- Control-flow statements.
breakStatement :: { StatementContentNode }: BREAK SEMICOLON {}
continueStatement: CONTINUE SEMICOLON {}
endStatement: END SEMICOLON {}
forStatement: FOR scalarType Identifier IN (setExpression | LBRACKET rangeExpression RBRACKET | expression) body=statementOrScope {}
ifStatement: IF LPAREN expression RPAREN if_body=statementOrScope (ELSE else_body=statementOrScope)? {}
returnStatement: RETURN (expression | measureExpression)? SEMICOLON {}
whileStatement: WHILE LPAREN expression RPAREN body=statementOrScope {}

-- Quantum directive statements.
barrierStatement: BARRIER gateOperandList? SEMICOLON {}
boxStatement: BOX designator? scope {}
delayStatement: DELAY designator gateOperandList? SEMICOLON {}
{- 'gateCallStatement'  is split in two to avoid a potential ambiguity with an
 - 'expressionStatement' that consists of a single function call.  The only
 - "gate" that can have no operands is 'gphase' with no control modifiers, and
 - 'gphase(pi);' looks grammatically identical to 'fn(pi);'.  We disambiguate by
 - having 'gphase' be its own token, and requiring that all other gate calls
 - grammatically have at least one qubit.  Strictly, as long as 'gphase' is a
 - separate token, ANTLR can disambiguate the statements by the definition
 - order, but this is more robust. -}
gateCallStatement:
    gateModifier* Identifier (LPAREN expressionList? RPAREN)? designator? gateOperandList SEMICOLON
    | gateModifier* GPHASE (LPAREN expressionList? RPAREN)? designator? gateOperandList? SEMICOLON
 {}
-- measureArrowAssignmentStatement also permits the case of not assigning the
-- result to any classical value too.
measureArrowAssignmentStatement: measureExpression (ARROW indexedIdentifier)? SEMICOLON {}
resetStatement: RESET gateOperand SEMICOLON {}

-- Primitive declaration statements.
aliasDeclarationStatement: LET Identifier EQUALS aliasExpression SEMICOLON {}
classicalDeclarationStatement: (scalarType | arrayType) Identifier (EQUALS declarationExpression)? SEMICOLON {}
constDeclarationStatement: CONST scalarType Identifier EQUALS declarationExpression SEMICOLON {}
ioDeclarationStatement: (INPUT | OUTPUT) (scalarType | arrayType) Identifier SEMICOLON {}
oldStyleDeclarationStatement: (CREG | QREG) Identifier designator? SEMICOLON {}
quantumDeclarationStatement: qubitType Identifier SEMICOLON {}

-- Declarations and definitions of higher-order objects.
defStatement: DEF Identifier LPAREN argumentDefinitionList? RPAREN returnSignature? scope {}
externStatement: EXTERN Identifier LPAREN externArgumentList? RPAREN returnSignature? SEMICOLON {}
gateStatement: GATE Identifier (LPAREN params=identifierList? RPAREN)? qubits=identifierList scope {}

-- Non-declaration assignments and calculations.
assignmentStatement: indexedIdentifier op=(EQUALS | CompoundAssignmentOperator) (expression | measureExpression) SEMICOLON {}
expressionStatement: expression SEMICOLON {}

-- Statements where the bulk is in the calibration language.
calStatement: CAL LBRACE CalibrationBlock? RBRACE {}
defcalStatement: DEFCAL defcalTarget (LPAREN defcalArgumentDefinitionList? RPAREN)? defcalOperandList returnSignature? LBRACE CalibrationBlock? RBRACE {}


{- End top-level statement definitions. -}
{- Start expression definitions. -}


-- ANTLR4 can handle direct left-recursive rules, and ambiguities are guaranteed
-- to resolve in the order of definition.  This means that the order of rules
-- here defines the precedence table, from most tightly binding to least.
expression:
    LPAREN expression RPAREN                                  # parenthesisExpression
    | expression indexOperator                                # indexExpression
    | <assoc=right> expression op=DOUBLE_ASTERISK expression  # powerExpression
    | op=(TILDE | EXCLAMATION_POINT | MINUS) expression       # unaryExpression
    | expression op=(ASTERISK | SLASH | PERCENT) expression   # multiplicativeExpression
    | expression op=(PLUS | MINUS) expression                 # additiveExpression
    | expression op=BitshiftOperator expression               # bitshiftExpression
    | expression op=ComparisonOperator expression             # comparisonExpression
    | expression op=EqualityOperator expression               # equalityExpression
    | expression op=AMPERSAND expression                      # bitwiseAndExpression
    | expression op=CARET expression                          # bitwiseXorExpression
    | expression op=PIPE expression                           # bitwiseOrExpression
    | expression op=DOUBLE_AMPERSAND expression               # logicalAndExpression
    | expression op=DOUBLE_PIPE expression                    # logicalOrExpression
    | (scalarType | arrayType) LPAREN expression RPAREN       # castExpression
    | DURATIONOF LPAREN scope RPAREN                          # durationofExpression
    | Identifier LPAREN expressionList? RPAREN                # callExpression
    | (
        Identifier
        | BinaryIntegerLiteral
        | OctalIntegerLiteral
        | DecimalIntegerLiteral
        | HexIntegerLiteral
        | FloatLiteral
        | ImaginaryLiteral
        | BooleanLiteral
        | BitstringLiteral
        | TimingLiteral
        | HardwareQubit
      )                                                       # literalExpression
    {}

-- Special-case expressions that are only valid in certain contexts.  These are
-- not in the expression tree, but can contain elements that are within it.
aliasExpression: expression (DOUBLE_PLUS expression)* {}
declarationExpression: arrayLiteral | expression | measureExpression {}
measureExpression: MEASURE gateOperand {}
rangeExpression: expression? COLON expression? (COLON expression)? {}
setExpression: LBRACE expression (COMMA expression)* COMMA? RBRACE {}
arrayLiteral: LBRACE (expression | arrayLiteral) (COMMA (expression | arrayLiteral))* COMMA? RBRACE {}

-- The general form is a comma-separated list of indexing entities.
-- 'setExpression' is only valid when being used as a single index: registers
-- can support it for creating aliases, but arrays cannot.
indexOperator:
    LBRACKET
    (
        setExpression
        | (expression | rangeExpression) (COMMA (expression | rangeExpression))* COMMA?
    )
    RBRACKET {}
-- Alternative form to 'indexExpression' for cases where an obvious l-value is
-- better grammatically than a generic expression.  Some current uses of this
-- rule may be better as 'expression', leaving the semantic analysis to later
-- (for example in gate calls).
indexedIdentifier: Identifier indexOperator* {}

{- End expression definitions. -}
{- Start type definitions. -}

returnSignature: ARROW scalarType {}
gateModifier: (
    INV
    | POW LPAREN expression RPAREN
    | (CTRL | NEGCTRL) (LPAREN expression RPAREN)?
) AT {}

scalarType:
    BIT designator?
    | INT designator?
    | UINT designator?
    | FLOAT designator?
    | ANGLE designator?
    | BOOL
    | DURATION
    | STRETCH
    | COMPLEX (LBRACKET scalarType RBRACKET)?
    {}
qubitType: QUBIT designator? {}
arrayType: ARRAY LBRACKET scalarType COMMA expressionList RBRACKET {}
arrayReferenceType: (READONLY | MUTABLE) ARRAY LBRACKET scalarType COMMA (expressionList | DIM EQUALS expression) RBRACKET {}

designator: LBRACKET expression RBRACKET {}

defcalTarget: MEASURE | RESET | DELAY | Identifier {}
defcalArgumentDefinition: expression | argumentDefinition {}
defcalOperand: HardwareQubit | Identifier {}
gateOperand: indexedIdentifier | HardwareQubit {}
externArgument: scalarType | arrayReferenceType | CREG designator? {}
argumentDefinition:
    scalarType Identifier
    | qubitType Identifier
    | (CREG | QREG) Identifier designator?
    | arrayReferenceType Identifier
    {}

argumentDefinitionList: argumentDefinition (COMMA argumentDefinition)* COMMA? {}
defcalArgumentDefinitionList: defcalArgumentDefinition (COMMA defcalArgumentDefinition)* COMMA? {}
defcalOperandList: defcalOperand (COMMA defcalOperand)* COMMA? {}
expressionList: expression (COMMA expression)* COMMA? {}
identifierList: Identifier (COMMA Identifier)* COMMA? {}
gateOperandList: gateOperand (COMMA gateOperand)* COMMA? {}
externArgumentList: externArgument (COMMA externArgument)* COMMA? {}

-}

{
parseError :: L.Lexeme -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.Lexeme -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

parseQasm3String :: String -> Either String ProgramNode
parseQasm3String programStr = L.runAlex programStr parseQasm3
}
