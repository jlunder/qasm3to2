{
module Qasm3Lexer where

import Data.Char (chr)
import Qasm3
import Debug.Trace (trace)

}

%wrapper "monad"

$letter                 = [A-Za-z]
-- fragment ValidUnicode: [\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}]; // valid unicode chars
$firstIdCharacter       = [$letter _] -- | $validUnicode
$generalIdCharacter     = [$firstIdCharacter 0-9]

@decimalIntegerLiteral  = ([0-9] "_"?)* [0-9]

@floatLiteralExponent   = [eE] [\+\-]? @decimalIntegerLiteral

@floatLiteral           =
                        -- 1_123e-3, 123e+4 or 123E5 (needs the exponent or it's just an integer)
                        @decimalIntegerLiteral @floatLiteralExponent
                        -- .1234_5678 or .1e3 (no digits before the dot)
                        | "." @decimalIntegerLiteral @floatLiteralExponent?
                        -- 123.456, 123. or 145.32e+1_00
                        | @decimalIntegerLiteral "." @decimalIntegerLiteral? @floatLiteralExponent?

@timeUnit               = "dt" | "ns" | "us" | "µs" | "ms" | "s"

OpenQASM3 :-

<0>                     [\ \t\r\n]+             { lexeme VersionIdentiferWhitespaceToken }
<0>                     "OPENQASM" / [^$generalIdCharacter]
                                                { (lexeme OpenqasmToken) `andBegin` version_identifier }

<version_identifier>    [\ \t\r\n]+             { lexeme VersionIdentiferWhitespaceToken }
<version_identifier>    [0-9]+ ("." [0-9]+)?    { lexemeCat VersionSpecifierToken }
<version_identifier>    ";"                     { begin default_mode }

<default_mode>          "def"                   { lexeme DefToken }
<default_mode>          "gate"                  { lexeme GateToken }
<default_mode>          "extern"                { lexeme ExternToken }
<default_mode>          "box"                   { lexeme BoxToken }
<default_mode>          "let"                   { lexeme LetToken }
<default_mode>          "break"                 { lexeme BreakToken }
<default_mode>          "continue"              { lexeme ContinueToken }
<default_mode>          "if"                    { lexeme IfToken }
<default_mode>          "else"                  { lexeme ElseToken }
<default_mode>          "end"                   { lexeme EndToken }
<default_mode>          "return"                { lexeme ReturnToken }
<default_mode>          "for"                   { lexeme ForToken }
<default_mode>          "while"                 { lexeme WhileToken }
<default_mode>          "in"                    { lexeme InToken }
<default_mode>          "input"                 { lexeme InputToken }
<default_mode>          "output"                { lexeme OutputToken }
<default_mode>          "const"                 { lexeme ConstToken }
<default_mode>          "readonly"              { lexeme ReadonlyToken }
<default_mode>          "mutable"               { lexeme MutableToken }
<default_mode>          "qreg"                  { lexeme QregToken }
<default_mode>          "qubit"                 { lexeme QubitToken }
<default_mode>          "creg"                  { lexeme CregToken }
<default_mode>          "bool"                  { lexeme BoolToken }
<default_mode>          "bit"                   { lexeme BitToken }
<default_mode>          "int"                   { lexeme IntToken }
<default_mode>          "uint"                  { lexeme UintToken }
<default_mode>          "float"                 { lexeme FloatToken }
<default_mode>          "angle"                 { lexeme AngleToken }
<default_mode>          "complex"               { lexeme ComplexToken }
<default_mode>          "array"                 { lexeme ArrayToken }
<default_mode>          "void"                  { lexeme VoidToken }
<default_mode>          "duration"              { lexeme DurationToken }
<default_mode>          "stretch"               { lexeme StretchToken }
<default_mode>          "gphase"                { lexeme GphaseToken }
<default_mode>          "inv"                   { lexeme InvToken }
<default_mode>          "pow"                   { lexeme PowToken }
<default_mode>          "ctrl"                  { lexeme CtrlToken }
<default_mode>          "negctrl"               { lexeme NegctrlToken }
<default_mode>          "#dim"                  { lexeme DimToken }
<default_mode>          "durationof"            { lexeme DurationofToken }
<default_mode>          "delay"                 { lexeme DelayToken }
<default_mode>          "reset"                 { lexeme ResetToken }
<default_mode>          "measure"               { lexeme MeasureToken }
<default_mode>          "barrier"               { lexeme BarrierToken }
<default_mode>          "true" | "false"        { lexemeCat BooleanLiteralToken }
<default_mode>          "["                     { lexeme LbracketToken }
<default_mode>          "]"                     { lexeme RbracketToken }
<default_mode>          "{"                     { lexeme LbraceToken }
<default_mode>          "}"                     { lexeme RbraceToken }
<default_mode>          "("                     { lexeme LparenToken }
<default_mode>          ")"                     { lexeme RparenToken }
<default_mode>          ":"                     { lexeme ColonToken }
<default_mode>          ";"                     { lexeme SemicolonToken }
<default_mode>          "."                     { lexeme DotToken }
<default_mode>          ","                     { lexeme CommaToken }
<default_mode>          "="                     { lexeme EqualsToken }
<default_mode>          "->"                    { lexeme ArrowToken }
<default_mode>          "+"                     { lexeme PlusToken }
<default_mode>          "++"                    { lexeme DoublePlusToken }
<default_mode>          "-"                     { lexeme MinusToken }
<default_mode>          "*"                     { lexeme AsteriskToken }
<default_mode>          "**"                    { lexeme DoubleAsteriskToken }
<default_mode>          "/"                     { lexeme SlashToken }
<default_mode>          "%"                     { lexeme PercentToken }
<default_mode>          "|"                     { lexeme PipeToken }
<default_mode>          "||"                    { lexeme DoublePipeToken }
<default_mode>          "&"                     { lexeme AmpersandToken }
<default_mode>          "&&"                    { lexeme DoubleAmpersandToken }
<default_mode>          "^"                     { lexeme CaretToken }
<default_mode>          "@"                     { lexeme AtToken }
<default_mode>          "~"                     { lexeme TildeToken }
<default_mode>          "!"                     { lexeme ExclamationPointToken }
<default_mode>          [\ \t]+                 { lexemeCat WhitespaceToken }
<default_mode>          [\r\n]+                 { lexemeCat NewlineToken }
<default_mode>          "//" ~[\r\n]*           { lexemeCat LineCommentToken }
<default_mode>          "/*" .* "*/"            { lexemeCat BlockCommentToken }
<default_mode>          "im"                    { lexeme ImagToken }
<default_mode>          "==" | "!="             { lexemeCat EqualityOperatorToken }
<default_mode>          "+=" | "-=" | "*=" | "/=" | "&=" | "|=" | "~=" | "^=" | "<<=" | ">>=" | "%=" | "**="
                                                { lexemeCat CompoundAssignmentOperatorToken }
<default_mode>          ">" | "<" | ">=" | "<=" { lexemeCat ComparisonOperatorToken }
<default_mode>          ">>" | "<<"             { lexemeCat BitshiftOperatorToken }
<default_mode>          (@decimalIntegerLiteral | @floatLiteral) [\ \t]* "im"
                                                { lexemeCat ImaginaryLiteralToken }
<default_mode>          ("0b" | "0B") ([01] "_"?)* [01]
                                                { lexemeCat BinaryIntegerLiteralToken }
<default_mode>          "0o" ([0-7] "_"?)* [0-7]
                                                { lexemeCat OctalIntegerLiteralToken }
<default_mode>          @decimalIntegerLiteral  { lexemeCat DecimalIntegerLiteralToken }
<default_mode>          ("0x" | "0X") ([0-9a-fA-F] "_"?)* [0-9a-fA-F]
                                                { lexemeCat HexIntegerLiteralToken }
<default_mode>          $firstIdCharacter $generalIdCharacter*
                                                { lexemeCat IdentifierToken }
<default_mode>          "$" [0-9]+              { lexemeCat HardwareQubitToken }

<default_mode>          "\"" ([01] "_"?)* [01] "\""
                                                { lexemeCat BitstringLiteralToken }

<default_mode>          @floatLiteral           { lexemeCat FloatLiteralToken }

-- represents explicit time value in SI or backend units
<default_mode>          (@decimalIntegerLiteral | @floatLiteral) @timeUnit
                                                { lexemeCat TimingLiteralToken }

-- // An include statement"s path or defcalgrammar target is potentially ambiguous
-- // with `BitstringLiteral`.
-- mode ARBITRARY_STRING;
--     ARBITRARY_STRING_WHITESPACE: [ \t\r\n]+ -> skip;
--     // allow ``"str"`` and ``"str"``;
--     StringLiteral: (""" ~["\r\t\n]+? """ | "\"" ~["\r\t\n]+? "\"") -> popMode;


-- <arbitrary_string>      $digit      ;
-- <eat_to_line_end>       $digit      ;
-- <cal_prelude>           $digit      ;
-- <cal_block>             $digit      ;
-- <defcal_prelude>        $digit      ;


-- DEFCALGRAMMAR: "defcalgrammar" -> pushMode(ARBITRARY_STRING);
-- CAL: "cal" -> mode(CAL_PRELUDE);
-- DEFCAL: "defcal" -> mode(DEFCAL_PRELUDE);
-- PRAGMA: "#"? "pragma" -> pushMode(EAT_TO_LINE_END);
-- AnnotationKeyword: "@" Identifier ->  pushMode(EAT_TO_LINE_END);


-- // A different lexer mode to swap to when we need handle tokens on a line basis
-- // rather than the default arbitrary-whitespace-based tokenisation.  This is
-- // used by the annotation and pragma rules.lexerCommentDepth
--     EAT_LINE_END: [\r\n] -> popMode, skip;

--     // The line content must be a non-empty token to satisfy ANTLR (otherwise it
--     // would be able to produce an infinite number of tokens).  We could include
--     // the line ending to guarantee that this is always non-empty, but that just
--     // puts an annoying burden on consumers to remove it again.
--     RemainingLineContent: ~[ \t\r\n] ~[\r\n]*;


-- // We need to do a little context-aware lexing when we hit a `cal` or `defcal`
-- // token.  In both cases, there's a small interlude before the pulse grammar
-- // block starts, and we need to be able to lex our way through that.  We don't
-- // want to tie this grammar to one host language by injecting host code to
-- // manage the state of the lexer, so instead we need to do a little duplication
-- // of the tokens, because ANTLR doesn't allow us to inherit rules directly.
-- mode CAL_PRELUDE;
--     CAL_PRELUDE_WHITESPACE: [ \t\r\n]+ -> skip;
--     CAL_PRELUDE_COMMENT: (LineComment | BlockComment) -> skip;
--     CAL_PRELUDE_LBRACE: LBRACE -> type(LBRACE), mode(CAL_BLOCK);

-- mode DEFCAL_PRELUDE;
--     DEFCAL_PRELUDE_WHITESPACE: [ \t\r\n]+ -> skip;
--     DEFCAL_PRELUDE_COMMENT: (LineComment | BlockComment) -> skip;
--     DEFCAL_PRELUDE_LBRACE: LBRACE -> type(LBRACE), mode(CAL_BLOCK);

--     // Duplications of valid constant expression tokens that may appear in the
--     // argument list.  This is an unfortunately large number of duplications.

--     // Types.
--     DEFCAL_PRELUDE_QREG: QREG -> type(QREG);
--     DEFCAL_PRELUDE_QUBIT: QUBIT -> type(QUBIT);
--     DEFCAL_PRELUDE_CREG: CREG -> type(CREG);
--     DEFCAL_PRELUDE_BOOL: BOOL -> type(BOOL);
--     DEFCAL_PRELUDE_BIT: BIT -> type(BIT);
--     DEFCAL_PRELUDE_INT: INT -> type(INT);
--     DEFCAL_PRELUDE_UINT: UINT -> type(UINT);
--     DEFCAL_PRELUDE_ANGLE: ANGLE -> type(ANGLE);
--     DEFCAL_PRELUDE_FLOAT: FLOAT -> type(FLOAT);
--     DEFCAL_PRELUDE_COMPLEX: COMPLEX -> type(COMPLEX);
--     DEFCAL_PRELUDE_ARRAY: ARRAY -> type(ARRAY);
--     DEFCAL_PRELUDE_DURATION: DURATION -> type(DURATION);
--     // Punctuation.
--     DEFCAL_PRELUDE_LBRACKET: LBRACKET -> type(LBRACKET);
--     DEFCAL_PRELUDE_RBRACKET: RBRACKET -> type(RBRACKET);
--     DEFCAL_PRELUDE_LPAREN: LPAREN -> type(LPAREN);
--     DEFCAL_PRELUDE_RPAREN: RPAREN -> type(RPAREN);
--     DEFCAL_PRELUDE_ARROW: ARROW -> type(ARROW);
--     DEFCAL_PRELUDE_COMMA: COMMA -> type(COMMA);
--     DEFCAL_PRELUDE_PLUS: PLUS -> type(PLUS);
--     DEFCAL_PRELUDE_MINUS: MINUS -> type(MINUS);
--     DEFCAL_PRELUDE_ASTERISK: ASTERISK -> type(ASTERISK);
--     DEFCAL_PRELUDE_SLASH: SLASH -> type(SLASH);
--     DEFCAL_PRELUDE_BitshiftOperator: BitshiftOperator -> type(BitshiftOperator);
--     // Literals and names.
--     DEFCAL_PRELUDE_BitstringLiteral: BitstringLiteral -> type(BitstringLiteral);
--     DEFCAL_PRELUDE_BinaryIntegerLiteral: BinaryIntegerLiteral -> type(BinaryIntegerLiteral);
--     DEFCAL_PRELUDE_OctalIntegerLiteral: OctalIntegerLiteral -> type(OctalIntegerLiteral);
--     DEFCAL_PRELUDE_DecimalIntegerLiteral: DecimalIntegerLiteral -> type(DecimalIntegerLiteral);
--     DEFCAL_PRELUDE_HexIntegerLiteral: HexIntegerLiteral -> type(HexIntegerLiteral);
--     DEFCAL_PRELUDE_FloatLiteral: FloatLiteral -> type(FloatLiteral);
--     DEFCAL_PRELUDE_MEASURE: MEASURE -> type(MEASURE);
--     DEFCAL_PRELUDE_DELAY: DELAY -> type(DELAY);
--     DEFCAL_PRELUDE_RESET: RESET -> type(RESET);
--     DEFCAL_PRELUDE_Identifier: Identifier -> type(Identifier);
--     DEFCAL_PRELUDE_HardwareQubit: HardwareQubit -> type(HardwareQubit);


-- // The meat-and-potatoes of matching a calibration block with balanced inner
-- // braces.  We enter `CAL_BLOCK` with the opening brace already tokenised
-- // (that's how the lexer knew to swap modes to us), and with the token left open
-- // to continue to accumulate.  We want to tokenise until we hit the balancing
-- // brace.  Since we have _no_ knowledge of what the inner langauge is doing,
-- // things like unbalanced braces in comments will cause a failure, but there's
-- // not much we can do about that without greater spec restrictions.
-- mode CAL_BLOCK;
--     fragment NestedCalibrationBlock: LBRACE (NestedCalibrationBlock | ~[{}])* RBRACE;
--     CalibrationBlock: (NestedCalibrationBlock | ~[{}])+;
--     CAL_BLOCK_RBRACE: RBRACE -> type(RBRACE), mode(DEFAULT_MODE);

{

data Lexeme = L AlexPosn Token deriving (Eq, Show)

lexeme :: Token -> AlexInput -> Int -> Alex Lexeme
-- lexeme token (_, _, _, str) len | trace ("lexeme " ++ show token ++ " from \"" ++ (take len str) ++ "\"") False = undefined
lexeme token (p, _, _, str) len = return (L p token)

lexemeCat :: (String -> Token) -> AlexInput -> Int -> Alex Lexeme
-- lexemeCat tokenCat (_, _, _, str) len | trace ("lexemeCat from \"" ++ (take len str) ++ "\"") False = undefined
lexemeCat tokenCat (p, _, _, str) len = return (L p $ tokenCat (take len str))

nested_comment :: AlexInput -> Int -> Alex Lexeme
nested_comment _ _ = do
  input <- alexGetInput
  go 1 input
  where
    go 0 input = do alexSetInput input; alexMonadScan
    go n input = do
      case alexGetByte input of
        Nothing -> err input
        Just (c, input) -> do
          case chr (fromIntegral c) of
            '-' -> do
              let temp = input
              case alexGetByte input of
                Nothing -> err input
                Just (125, input) -> go (n - 1) input
                Just (45, input) -> go n temp
                Just (c, input) -> go n input
            '\123' -> do
              case alexGetByte input of
                Nothing -> err input
                Just (c, input) | c == fromIntegral (ord '-') -> go (n + 1) input
                Just (c, input) -> go n input
            c -> go n input
    err input = do alexSetInput input; lexError "error in nested comment"

lexError s = do
  (p, c, _, input) <- alexGetInput
  alexError
    ( showPosn p
        ++ ": "
        ++ s
        ++ ( if (not (null input))
               then " before " ++ show (head input)
               else " at end of file"
           )
    )

scanner str = runAlex str $ do
  let loop lexemes = do
        tok@lexeme <- alexMonadScan
        case lexeme of
          (L _ EofToken) -> return lexemes
          (L _ _) -> do loop $! lexemes ++ [lexeme]
  loop []

alexEOF = return (L undefined EofToken)

showPosn (AlexPn _ line col) = show line ++ ':' : show col

}
