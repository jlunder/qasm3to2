{
module Qasm3Parser (parseQasm3, parseQasm3String) where

import Ast
import Control.Monad (mplus)
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
    -- VOID                                    { Lexeme _ VoidToken }
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
    -- DOT                                     { Lexeme _ DotToken }
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
    -- Whitespace                              { Lexeme _ (WhitespaceToken _) }
    -- Newline                                 { Lexeme _ (NewlineToken _) }
    -- LineComment                             { Lexeme _ (LineCommentToken _) }
    -- BlockComment                            { Lexeme _ (BlockCommentToken _) }
    VersionSpecifier                        { Lexeme _ (VersionSpecifierToken _) }
    StringLiteral                           { Lexeme _ (StringLiteralToken _) }
    RemainingLineContent                    { Lexeme _ (RemainingLineContentToken _) }
    CalibrationBlock                        { Lexeme _ (CalibrationBlockToken _) }


-- Specify binary operator associativity and precedence: lower precedence first (opposite to ANTLR)

%nonassoc FOR
%nonassoc THEN
%nonassoc ELSE

%left DOUBLE_PIPE             -- logicalOrExpression
%left DOUBLE_AMPERSAND        -- logicalAndExpression
%left PIPE                    -- bitwiseOrExpression
%left CARET                   -- bitwiseXorExpression
%left AMPERSAND               -- bitwiseAndExpression
%left EqualityOperator        -- equalityExpression
%left ComparisonOperator      -- comparisonExpression
%left BitshiftOperator        -- bitshiftExpression
%left PLUS MINUS              -- additiveExpression
%left ASTERISK SLASH PERCENT  -- multiplicativeExpression
%left TILDE EXCLAMATION_POINT UNARY_MINUS
%right DOUBLE_ASTERISK        -- powerExpression
%left LBRACKET RBRACKET LPAREN RPAREN

%nonassoc RVALUE_INDEX
%nonassoc LVALUE_INDEX


%%


program :: { ProgramNode }
    : OPENQASM VersionSpecifier SEMICOLON many0(statement)
                                    { Program $1 $2 $4 }

-- A statement is any valid single statement of an OpenQASM 3 program, with the
-- exception of the version-definition statement (which must be unique, and the
-- first statement of the file if present).  This file just defines rules for
-- parsing; we leave semantic analysis and rejection of invalid scopes for
-- compiler implementations.
statement :: { StatementNode }
    : PRAGMA RemainingLineContent   { Pragma $1 $2 }
    -- All the actual statements of the language.
    | many0(annotation) statementContent
                                    { Annotated $1 $2 }

annotation :: { AnnotationNode }
    : AnnotationKeyword RemainingLineContent
                                    { Annotation $1 $2 }

scope :: { [StatementNode] }
    : LBRACE many0(statement) RBRACE
                                    { $2 }

statementOrScope :: { StatementOrScopeNode }
    : statement { Statement $1 }
    | scope { Scope $1 }


{- Start top-level statement definitions. -}

statementContent :: { StatementContentNode }
-- Inclusion statements.
    : DEFCALGRAMMAR StringLiteral SEMICOLON
                                    { Cal $1 $2 }
    | INCLUDE StringLiteral SEMICOLON
                                    { Include $1 $2 }

-- Control-flow statements.
    | BREAK SEMICOLON               { Break $1 }
    | CONTINUE SEMICOLON            { Continue $1 }
    | END SEMICOLON                 { End $1 }

    -- | FOR scalarType Identifier IN expression statementOrScope
    --                                 { For $1 $2 $3 $5 $6 }
    | FOR scalarType Identifier IN lvalueExpression statement
                                    { For $1 $2 $3 (toExpression $5) (Statement $6) }
    | FOR scalarType Identifier IN lvalueExpression scope
                                    { For $1 $2 $3 (toExpression $5) (Scope $6) }

    | FOR scalarType Identifier IN LBRACKET rangeExpression RBRACKET statementOrScope
                                    { RangeFor $1 $2 $3 $6 $8 }
    | FOR scalarType Identifier IN setExpression statementOrScope
                                    { SetFor $1 $2 $3 $5 $6 }
    | IF LPAREN expression RPAREN statementOrScope ifElseClause
                                    { If $1 $3 $5 $6 }

    | RETURN opt(measureExpression) SEMICOLON
                                    { Return $1 $2 }
    | WHILE LPAREN expression RPAREN statementOrScope
                                    { While $1 $3 $5 }

-- Quantum directive statements.
    | BARRIER list0(gateOperand) SEMICOLON
                                    { Barrier $1 $2 }
    | BOX opt(designator) scope     { Box $1 $2 $3 }
    | DELAY designator list0(gateOperand) SEMICOLON
                                    { Delay $1 $2 $3 }

{- 'gateCallStatement'  is split in two to avoid a potential ambiguity with an
 - 'expressionStatement' that consists of a single function call.  The only
 - "gate" that can have no operands is 'gphase' with no control modifiers, and
 - 'gphase(pi);' looks grammatically identical to 'fn(pi);'.  We disambiguate by
 - having 'gphase' be its own token, and requiring that all other gate calls
 - grammatically have at least one qubit.  Strictly, as long as 'gphase' is a
 - separate token, ANTLR can disambiguate the statements by the definition
 - order, but this is more robust. -}

    -- Original ANTLR grammar:
    -- gateModifierList Identifier ((LPAREN) expressionList (RPAREN))? designator? gateOperandList? (SEMICOLON)

    -- My naive translation:
    -- | many(gateModifier) Identifier optParen(list0(expression)) opt(designator) list1(gateOperand) SEMICOLON
                                    -- { GateCall $1 $2 (maybe [] id $3) $4 $5 }
    -- | many(gateModifier) GPHASE optParen(list0(expression)) opt(designator) list0(gateOperand) SEMICOLON
    --                                 { GateCall $1 $2 (maybe [] id $3) $4 $5 }

    -- The rules are further subdivided because having a zero-length production
    -- at the start of these rules prevents them from being merged with rules
    -- that share a common prefix (i.e., the Expression and Assignment rules
    -- below, both of which can start with Identifier). Without going into too
    -- much detail, the problem is that without arbitrary lookahead, when the
    -- parser encounters the "Identifier" token, it can't decide whether to
    -- produce a zero-length gateModifier list before it, i.e., it has to
    -- decide right then whether it's generating a GateCall or Expression. If
    -- there's a rule for GateCall that doesn't include the zero-length
    -- gateModifier list, then it can carry on reading tokens for a while
    -- longer before it decides which rule to reduce.
    | Identifier list1(gateOperand) SEMICOLON
                                    { GateCall [] $1 [] Nothing $2 }
    | Identifier LPAREN list0(expression) RPAREN list1(gateOperand) SEMICOLON
                                    { GateCall [] $1 $3 Nothing $5 }
    | many1(gateModifier) Identifier LPAREN list0(expression) RPAREN list1(gateOperand) SEMICOLON
                                    { GateCall $1 $2 $4 Nothing $6 }
    | GPHASE optParen(list0(expression)) list0(gateOperand) SEMICOLON
                                    { GateCall [] $1 (maybe [] id $2) Nothing $3 }
    | many1(gateModifier) GPHASE optParen(list0(expression)) list0(gateOperand) SEMICOLON
                                    { GateCall $1 $2 (maybe [] id $3) Nothing $4 }

-- measureArrowAssignmentStatement also permits the case of not assigning the
-- result to any classical value too.
    | MEASURE gateOperand opt(measureArrowTarget) SEMICOLON
                                    { MeasureArrowAssignment $1 $2 $3 }
    | RESET gateOperand SEMICOLON   { Reset $1 $2 }

-- Primitive declaration statements.
    | LET Identifier EQUALS aliasExpression SEMICOLON
                                    { AliasDeclaration $1 $2 $4 }
    | scalarOrArrayType Identifier opt(declarationExpression) SEMICOLON
                                    { ClassicalDeclaration $1 $2 $3 }
    | CONST scalarType Identifier declarationExpression SEMICOLON
                                    { ConstDeclaration $1 $2 $3 $4 }
    | INPUT scalarOrArrayType Identifier SEMICOLON
                                    { InputIoDeclaration $1 $2 $3 }
    | OUTPUT scalarOrArrayType Identifier SEMICOLON
                                    { OutputIoDeclaration $1 $2 $3 }
    | CREG Identifier opt(designator) SEMICOLON
                                    { CregOldStyleDeclaration $1 $2 $3 }
    | QREG Identifier opt(designator) SEMICOLON
                                    { QregOldStyleDeclaration $1 $2 $3 }
    | qubitType Identifier SEMICOLON
                                    { QuantumDeclaration $1 $2 }

-- Declarations and definitions of higher-order objects.
    | DEF Identifier LPAREN list0(argumentDefinition) RPAREN opt(returnSignature) scope
                                    { Def $1 $2 $4 $6 $7 }
    | EXTERN Identifier LPAREN list0(externArgument) RPAREN opt(returnSignature) SEMICOLON
                                    { Extern $1 $2 $4 $6 }
    | GATE Identifier optParen(list0(Identifier)) list0(Identifier) scope
                                    { Gate $1 $2 (maybe [] id $3) $4 $5 }

-- Non-declaration assignments and calculations.
    | lvalueExpression EQUALS measureExpression SEMICOLON
                                    { Assignment $1 $2 $3 }
    | lvalueExpression CompoundAssignmentOperator measureExpression SEMICOLON
                                    { Assignment $1 $2 $3 }

    | expression SEMICOLON          { Expression $1 }

-- Statements where the bulk is in the calibration language.
    | CAL calibrationBlock
                                    { Cal $1 $2 }
    | DEFCAL defcalTarget optParen(list0(defcalArgumentDefinition)) list0(defcalOperand) opt(returnSignature)
      calibrationBlock
                                    { Defcal $1 $2 (maybe [] id $3) $4 $5 $6 }

ifElseClause :: { Maybe StatementOrScopeNode }
    : %prec THEN                    { Nothing }
    | ELSE statementOrScope         { Just $2 }

measureArrowTarget :: { IndexedIdentifierNode }
    : ARROW lvalueExpression        { $2 }

scalarOrArrayType :: { ScalarOrArrayTypeNode }
    : scalarType                    { Scalar $1 }
    | arrayType                     { Array $1 }

calibrationBlock :: { Lexeme }
    : LBRACE many0(calibrationElement) RBRACE
                                    { mergeCalibrationBlock (foldl mergeCalibrationBlock $1 $2) $3 }

calibrationElement :: { Lexeme }
    : CalibrationBlock              { $1 }
    | calibrationBlock              { $1 }

{- End top-level statement definitions. -}


{- Start expression definitions. -}

-- Operator precedence is resolved in the top section of the Happy definition.
expression :: { ExpressionNode }
    : LPAREN expression RPAREN      { $2 }
    | expression DOUBLE_PIPE expression
                                    { BinaryOperatorExpression $1 $2 $3 }
    | expression DOUBLE_AMPERSAND expression
                                    { BinaryOperatorExpression $1 $2 $3 }
    | expression PIPE expression
                                    { BinaryOperatorExpression $1 $2 $3 }
    | expression CARET expression
                                    { BinaryOperatorExpression $1 $2 $3 }
    | expression AMPERSAND expression
                                    { BinaryOperatorExpression $1 $2 $3 }
    | expression EqualityOperator expression
                                    { BinaryOperatorExpression $1 $2 $3 }
    | expression ComparisonOperator expression
                                    { BinaryOperatorExpression $1 $2 $3 }
    | expression BitshiftOperator expression
                                    { BinaryOperatorExpression $1 $2 $3 }
    | expression PLUS expression    { BinaryOperatorExpression $1 $2 $3 }
    | expression MINUS expression   { BinaryOperatorExpression $1 $2 $3 }
    | expression ASTERISK expression
                                    { BinaryOperatorExpression $1 $2 $3 }
    | expression SLASH expression   { BinaryOperatorExpression $1 $2 $3 }
    | expression PERCENT expression { BinaryOperatorExpression $1 $2 $3 }
    | expression DOUBLE_ASTERISK expression
                                    { BinaryOperatorExpression $1 $2 $3 }
    | TILDE expression              { UnaryOperatorExpression $1 $2 }
    | EXCLAMATION_POINT expression  { UnaryOperatorExpression $1 $2 }
    | MINUS expression %prec UNARY_MINUS
                                    { UnaryOperatorExpression $1 $2 }
    | expression indexOperator %prec RVALUE_INDEX
                                    { IndexExpression $1 $2 }
    | lvalueExpression %prec LVALUE_INDEX
                                    { toExpression $1 }
    | scalarOrArrayType LPAREN expression RPAREN
                                    { CastExpression $1 $3 }
    | DURATIONOF LPAREN scope RPAREN
                                    { DurationOfExpression $1 $3 }
    | Identifier LPAREN list0(expression) RPAREN
                                    { CallExpression $1 $3 }
    | BinaryIntegerLiteral          { BinaryIntegerLiteral $1 }
    | OctalIntegerLiteral           { OctalIntegerLiteral $1 }
    | DecimalIntegerLiteral         { DecimalIntegerLiteral $1 }
    | HexIntegerLiteral             { HexIntegerLiteral $1 }
    | FloatLiteral                  { FloatLiteral $1 }
    | ImaginaryLiteral              { ImaginaryLiteral $1 }
    | BooleanLiteral                { BooleanLiteral $1 }
    | BitstringLiteral              { BitstringLiteral $1 }
    | TimingLiteral                 { TimingLiteral $1 }
    | HardwareQubit                 { HardwareQubitLiteral $1 }

-- Special-case expressions that are only valid in certain contexts.  These are
-- not in the expression tree, but can contain elements that are within it.
aliasExpression :: { AliasExpressionNode }
    : listSep1(DOUBLE_PLUS, expression)
                                    { AliasExpression $1 }

declarationExpression :: { DeclarationExpressionNode }
    : EQUALS arrayLiteral           { ArrayLiteralDeclarationExpression $2 }
    | EQUALS measureExpression      { ExpressionDeclarationExpression $2 }

measureExpression :: { MeasureExpressionNode }
    : expression                    { PlainExpression $1 }
    | MEASURE gateOperand           { MeasureExpression $1 $2 }

rangeOrExpressionIndex :: { RangeOrExpressionIndexNode }
    : expression                    { ExpressionIndex $1 }
    | rangeExpression               { RangeIndex $1 }

rangeExpression :: { RangeExpressionNode }
    : opt(expression) COLON opt(expression) opt2(COLON, expression)
                                    { RangeExpression (maybe (sourceRef $2) sourceRef $1) $1 $3 $4 }

setExpression :: { SetExpressionNode }
    : LBRACE list0(expression) RBRACE
                                    { SetExpression $2 }

arrayLiteral :: { ArrayLiteralNode }
    : LBRACE list0(arrayLiteralElement) RBRACE
                                    { ArrayLiteral $2 }

arrayLiteralElement :: { ArrayLiteralElementNode }
    : expression                    { ExpressionArrayElement $1 }
    | arrayLiteral                  { ArrayArrayElement $1 }

-- The general form is a comma-separated list of indexing entities.
-- 'setExpression' is only valid when being used as a single index: registers
-- can support it for creating aliases, but arrays cannot.
indexOperator :: { IndexOperatorNode }
    : LBRACKET setExpression RBRACKET
                                    { SetIndex $2 }
    | LBRACKET list0(rangeOrExpressionIndex) RBRACKET
                                    { IndexList $2 }

-- Alternative form to 'indexExpression' for cases where an obvious l-value is
-- better grammatically than a generic expression.  Some current uses of this
-- rule may be better as 'expression', leaving the semantic analysis to later
-- (for example in gate calls).
lvalueExpression :: { IndexedIdentifierNode }
    : Identifier                    { IndexedIdentifier $1 [] }
    | lvalueExpression indexOperator
                                    { appendIndexOperator $1 $2 }

{- End expression definitions. -}


{- Start type definitions. -}

returnSignature :: { ScalarTypeNode }
    : ARROW scalarType              { $2 }

gateModifier :: { GateModifierNode }
    : INV AT                        { InvGateModifier $1 }
    | POW LPAREN expression RPAREN AT
                                    { PowGateModifier $1 $3 }
    | CTRL optParen(expression) AT  { CtrlGateModifier $1 $2 }
    | NEGCTRL optParen(expression) AT
                                    { NegCtrlGateModifier $1 $2 }

scalarType :: { ScalarTypeNode }
    : BIT opt(designator)           { BitType $1 $2 }
    | INT opt(designator)           { IntType $1 $2 }
    | UINT opt(designator)          { UintType $1 $2 }
    | FLOAT opt(designator)         { FloatType $1 $2 }
    | ANGLE opt(designator)         { AngleType $1 $2 }
    | BOOL                          { BoolType $1 }
    | DURATION                      { DurationType $1 }
    | STRETCH                       { StretchType $1 }
    | COMPLEX opt3(LBRACKET, scalarType, RBRACKET)
                                    { ComplexType $1 $2 }

qubitType :: { QubitTypeNode }
    : QUBIT opt(designator)         { QubitType $1 $2 }

arrayType :: { ArrayTypeNode }
    : ARRAY LBRACKET scalarType COMMA list0(expression) RBRACKET
                                    { ArrayType $1 $3 $5 }

arrayReferenceType :: { ArrayReferenceTypeNode }
    : READONLY ARRAY LBRACKET scalarType COMMA list0(expression) RBRACKET
                                    { ReadonlyArrayReferenceType $1 $4 $6 }
    | MUTABLE ARRAY LBRACKET scalarType COMMA list0(expression) RBRACKET
                                    { MutableArrayReferenceType $1 $4 $6 }
    | READONLY ARRAY LBRACKET scalarType COMMA DIM EQUALS expression RBRACKET
                                    { ReadonlyArrayReferenceDimType $1 $4 $8 }
    | MUTABLE ARRAY LBRACKET scalarType COMMA DIM EQUALS expression RBRACKET
                                    { MutableArrayReferenceDimType $1 $4 $8 }

{- Start miscellany. -}

designator :: { ExpressionNode }
    : LBRACKET expression RBRACKET  { $2 }

defcalTarget :: { DefcalTargetNode }
    : MEASURE                       { MeasureDefcalTarget $1 }
    | RESET                         { ResetDefcalTarget $1 }
    | DELAY                         { DelayDefcalTarget $1 }
    | Identifier                    { IdentifierDefcalTarget $1 }

defcalArgumentDefinition :: { DefcalArgumentDefinitionNode }
    : expression                    { ExpressionDefcalArgument $1 }
    | argumentDefinition            { ArgumentDefinitionDefcalArgument $1 }

defcalOperand :: { DefcalOperandNode }
    : Identifier                    { IdentifierDefcal $1 }
    | HardwareQubit                 { HardwareQubitDefcal $1 }

gateOperand :: { GateOperandNode }
    : lvalueExpression              { IdentifierGateOperand $1}
    | HardwareQubit                 { HardwareQubitGateOperand $1 }

externArgument :: { ExternArgumentNode }
    : scalarType                    { ScalarExternArgument $1 }
    | arrayReferenceType            { ArrayExternArgument $1 }
    | CREG opt(designator)          { CregExternArgument $1 $2 }

argumentDefinition :: { ArgumentDefinitionNode }
    : scalarType Identifier         { ScalarArgument $1 $2 }
    | qubitType Identifier          { QubitArgument $1 $2 }
    | CREG Identifier opt(designator)
                                    { CregArgument $1 $2 $3 }
    | QREG Identifier opt(designator)
                                    { QregArgument $1 $2 $3 }
    | arrayReferenceType Identifier { ArrayArgument $1 $2 }

{- End miscellany. -}

{- End type definitions. -}


{- Start utility macros. -}

many0(p)
    :                               { [] }
    | manyRev1(p)                   { reverse $1 }

many1(p)
    : manyRev1(p)                   { reverse $1 }

manyRev1(p)
    : p                             { [$1] }
    | manyRev1(p) p                 { $2 : $1 }

list0(p)
    :                               { [] }
    | list1(p)                      { $1 }

-- Convention in this grammar is that comma-separated lists can have trailing commas.
list1(p)
    : p listSepRev(COMMA, p) opt(COMMA)
                                    { $1 : reverse $2 }

listSep1(s, p)
    : p listSepRev(s, p)
                                    { $1 : reverse $2 }

listSepRev(s, p)
    :                               { [] }
    | listSepRev(s, p) s p          { $3 : $1 }

opt(p)
    :                               { Nothing }
    | p                             { Just $1 }

opt2(p, q)
    :                               { Nothing }
    | p q                           { Just $2 }

opt3(p, q, r)
    :                               { Nothing }
    | p q r                         { Just $2 }

optParen(p)
    :                               { Nothing }
    | LPAREN p RPAREN               { Just $2 }

{- End utility macros. -}

{
mergeCalibrationBlock :: Lexeme -> Lexeme -> Lexeme
mergeCalibrationBlock (Lexeme refA tokA) (Lexeme refB tokB) =
  Lexeme (mplus refA refB) (CalibrationBlockToken (pretty tokA ++ pretty tokB))

toExpression :: IndexedIdentifierNode -> ExpressionNode
toExpression (IndexedIdentifier ident indices) =
  let wrap expr [] = expr
      wrap expr (index : indices) = wrap (IndexExpression expr index) indices
   in wrap (Identifier ident) (reverse indices)

appendIndexOperator :: IndexedIdentifierNode -> IndexOperatorNode -> IndexedIdentifierNode
appendIndexOperator (IndexedIdentifier ident indices) index = IndexedIdentifier ident (indices ++ [index])

parseError :: L.Lexeme -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.Lexeme -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

parseQasm3String :: String -> Either String ProgramNode
parseQasm3String programStr = L.runAlex programStr parseQasm3
}
