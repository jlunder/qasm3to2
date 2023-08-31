{
module Qasm3Lexer (Alex, AlexPosn(..), Lexeme, alexError, alexGetInput, alexMonadScan, runAlex, scanner) where

import Ast
import Data.Char (chr)
import Debug.Trace (trace)
import Qasm3
}

%wrapper "monad"

$letter                 = [A-Za-z]
-- fragment ValidUnicode: [\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}]; // valid unicode chars
$firstIdCharacter       = [$letter _] -- | $validUnicode
$generalIdCharacter     = [$firstIdCharacter 0-9]
$dquote                 = \"
$squote                 = \'
$inlineSpace            = [\ \t]
$newlineSpace           = [\r\n]
$space                  = [$inlineSpace$newlineSpace]

@decimalIntegerLiteral  = ([0-9] "_"?)* [0-9]

@floatLiteralExponent   = [eE] [\+\-]? @decimalIntegerLiteral

@floatLiteral           =
                        (
                          -- 1_123e-3, 123e+4 or 123E5 (needs the exponent or it's just an integer)
                          @decimalIntegerLiteral @floatLiteralExponent
                          -- .1234_5678 or .1e3 (no digits before the dot)
                          | "." @decimalIntegerLiteral @floatLiteralExponent?
                          -- 123.456, 123. or 145.32e+1_00
                          | @decimalIntegerLiteral "." @decimalIntegerLiteral? @floatLiteralExponent?
                        )
                        

@imaginaryLiteral       = @floatLiteral $inlineSpace* "im"

@timeUnit               = "dt" | "ns" | "us" | "µs" | "ms" | "s"

OpenQASM3 :-

<0>                     $space+                 ;
<0>                     "OPENQASM" / ~$generalIdCharacter
                                                { (makeLexeme OpenqasmToken) `andBegin` version_identifier }

<version_identifier>    $space+                 ;
<version_identifier>    [0-9]+ ("." [0-9]+)? / ~$generalIdCharacter
                                                { makeLexemeCat VersionSpecifierToken }
<version_identifier>    ";"                     { (makeLexeme SemicolonToken) `andBegin` default_mode }

<default_mode>          "def"                   { makeLexeme DefToken }
<default_mode>          "gate"                  { makeLexeme GateToken }
<default_mode>          "extern"                { makeLexeme ExternToken }
<default_mode>          "box"                   { makeLexeme BoxToken }
<default_mode>          "let"                   { makeLexeme LetToken }
<default_mode>          "break"                 { makeLexeme BreakToken }
<default_mode>          "continue"              { makeLexeme ContinueToken }
<default_mode>          "if"                    { makeLexeme IfToken }
<default_mode>          "else"                  { makeLexeme ElseToken }
<default_mode>          "end"                   { makeLexeme EndToken }
<default_mode>          "return"                { makeLexeme ReturnToken }
<default_mode>          "for"                   { makeLexeme ForToken }
<default_mode>          "while"                 { makeLexeme WhileToken }
<default_mode>          "in"                    { makeLexeme InToken }
<default_mode>          "input"                 { makeLexeme InputToken }
<default_mode>          "output"                { makeLexeme OutputToken }
<default_mode>          "const"                 { makeLexeme ConstToken }
<default_mode>          "readonly"              { makeLexeme ReadonlyToken }
<default_mode>          "mutable"               { makeLexeme MutableToken }
<default_mode>          "qreg"                  { makeLexeme QregToken }
<default_mode>          "qubit"                 { makeLexeme QubitToken }
<default_mode>          "creg"                  { makeLexeme CregToken }
<default_mode>          "bool"                  { makeLexeme BoolToken }
<default_mode>          "bit"                   { makeLexeme BitToken }
<default_mode>          "int"                   { makeLexeme IntToken }
<default_mode>          "uint"                  { makeLexeme UintToken }
<default_mode>          "float"                 { makeLexeme FloatToken }
<default_mode>          "angle"                 { makeLexeme AngleToken }
<default_mode>          "complex"               { makeLexeme ComplexToken }
<default_mode>          "array"                 { makeLexeme ArrayToken }
<default_mode>          "void"                  { makeLexeme VoidToken } -- unused
<default_mode>          "duration"              { makeLexeme DurationToken }
<default_mode>          "stretch"               { makeLexeme StretchToken }
<default_mode>          "gphase"                { makeLexeme GphaseToken }
<default_mode>          "inv"                   { makeLexeme InvToken }
<default_mode>          "pow"                   { makeLexeme PowToken }
<default_mode>          "ctrl"                  { makeLexeme CtrlToken }
<default_mode>          "negctrl"               { makeLexeme NegctrlToken }
<default_mode>          "#dim"                  { makeLexeme DimToken }
<default_mode>          "durationof"            { makeLexeme DurationofToken }
<default_mode>          "delay"                 { makeLexeme DelayToken }
<default_mode>          "reset"                 { makeLexeme ResetToken }
<default_mode>          "measure"               { makeLexeme MeasureToken }
<default_mode>          "barrier"               { makeLexeme BarrierToken }
<default_mode>          "true" | "false"        { makeLexemeCat BooleanLiteralToken }
<default_mode>          "["                     { makeLexeme LbracketToken }
<default_mode>          "]"                     { makeLexeme RbracketToken }
<default_mode>          "{"                     { makeLexeme LbraceToken }
<default_mode>          "}"                     { makeLexeme RbraceToken }
<default_mode>          "("                     { makeLexeme LparenToken }
<default_mode>          ")"                     { makeLexeme RparenToken }
<default_mode>          ":"                     { makeLexeme ColonToken }
<default_mode>          ";"                     { makeLexeme SemicolonToken }
<default_mode>          "."                     { makeLexeme DotToken } -- unused
<default_mode>          ","                     { makeLexeme CommaToken }
<default_mode>          "="                     { makeLexeme EqualsToken }
<default_mode>          "->"                    { makeLexeme ArrowToken }
<default_mode>          "+"                     { makeLexeme PlusToken }
<default_mode>          "++"                    { makeLexeme DoublePlusToken }
<default_mode>          "-"                     { makeLexeme MinusToken }
<default_mode>          "*"                     { makeLexeme AsteriskToken }
<default_mode>          "**"                    { makeLexeme DoubleAsteriskToken }
<default_mode>          "/"                     { makeLexeme SlashToken }
<default_mode>          "%"                     { makeLexeme PercentToken }
<default_mode>          "|"                     { makeLexeme PipeToken }
<default_mode>          "||"                    { makeLexeme DoublePipeToken }
<default_mode>          "&"                     { makeLexeme AmpersandToken }
<default_mode>          "&&"                    { makeLexeme DoubleAmpersandToken }
<default_mode>          "^"                     { makeLexeme CaretToken }
<default_mode>          "@"                     { makeLexeme AtToken }
<default_mode>          "~"                     { makeLexeme TildeToken }
<default_mode>          "!"                     { makeLexeme ExclamationPointToken }
<default_mode>          $inlineSpace+           ; -- { makeLexemeCat WhitespaceToken }
<default_mode>          $newlineSpace+          ; -- { makeLexeme NewlineToken }
<default_mode>          "//" ~$newlineSpace*    { makeLexemeCat LineCommentToken }
<default_mode>          "/*" .* "*/"            { makeLexemeCat BlockCommentToken }
<default_mode>          "==" | "!="             { makeLexemeCat EqualityOperatorToken }
<default_mode>          "+=" | "-=" | "*=" | "/=" | "&=" | "|=" | "~=" | "^=" | "<<=" | ">>=" | "%=" | "**="
                                                { makeLexemeCat CompoundAssignmentOperatorToken }
<default_mode>          ">" | "<" | ">=" | "<=" { makeLexemeCat ComparisonOperatorToken }
<default_mode>          ">>" | "<<"             { makeLexemeCat BitshiftOperatorToken }
<default_mode>          @imaginaryLiteral / ~$generalIdCharacter
                                                { makeLexemeCat ImaginaryLiteralToken }
<default_mode>          ("0b" | "0B") ([01] "_"?)* [01]
                                                { makeLexemeCat BinaryIntegerLiteralToken }
<default_mode>          "0o" ([0-7] "_"?)* [0-7]
                                                { makeLexemeCat OctalIntegerLiteralToken }
<default_mode>          @decimalIntegerLiteral / ~$generalIdCharacter
                                                { makeLexemeCat DecimalIntegerLiteralToken }
<default_mode>          ("0x" | "0X") ([0-9a-fA-F] "_"?)* [0-9a-fA-F]
                                                { makeLexemeCat HexIntegerLiteralToken }
<default_mode>          $firstIdCharacter $generalIdCharacter*
                                                { makeLexemeCat IdentifierToken }
<default_mode>          "$" [0-9]+              { makeLexemeCat HardwareQubitToken }

<default_mode>          "\"" ([01] "_"?)* [01] "\""
                                                { makeLexemeCat BitstringLiteralToken }

<default_mode>          @floatLiteral / ~$generalIdCharacter
                                                { makeLexemeCat FloatLiteralToken }

-- represents explicit time value in SI or backend units
<default_mode>          (@decimalIntegerLiteral | @floatLiteral) $inlineSpace* @timeUnit / ~$generalIdCharacter
                                                { makeLexemeCat TimingLiteralToken }

<default_mode>          "defcalgrammar"         { (makeLexeme DefcalgrammarToken) `andBegin` arbitrary_string }
<default_mode>          "cal"                   { (makeLexeme CalToken) `andBegin` cal_prelude }
<default_mode>          "defcal"                { (makeLexeme DefcalToken) `andBegin` cal_prelude }
<default_mode>          "#"? "pragma"           { (makeLexeme PragmaToken) `andBegin` eat_to_line_end }
<default_mode>          "@" Identifier          { (makeLexemeCat AnnotationKeywordToken) `andBegin` eat_to_line_end }

<arbitrary_string>      $space+                 ;
<arbitrary_string>      ($dquote ~[$newlineSpace $dquote]* $dquote | $squote ~[$newlineSpace $squote]* $squote)
                                                { (makeLexemeCat StringLiteralToken) `andBegin` default_mode }

-- A different lexer mode to swap to when we need handle tokens on a line basis
-- rather than the default arbitrary-whitespace-based tokenisation.  This is
-- used by the annotation and pragma rules.
<eat_to_line_end>       "\r"? "\n"              { begin default_mode } -- EAT_LINE_END

-- The line content must be a non-empty token to satisfy ANTLR (otherwise it
-- would be able to produce an infinite number of tokens).  We could include
-- the line ending to guarantee that this is always non-empty, but that just
-- puts an annoying burden on consumers to remove it again.
<eat_to_line_end>       ~[ \t\r\n] ~[\r\n]* / "\r"? "\n"
                                                { makeLexemeCat RemainingLineContentToken }

-- We need to do a little context-aware lexing when we hit a 'cal' or 'defcal'
-- token.  In both cases, there's a small interlude before the pulse grammar
-- block starts, and we need to be able to lex our way through that.  We don't
-- want to tie this grammar to one host language by injecting host code to
-- manage the state of the lexer, so instead we need to do a little duplication
-- of the tokens, because ANTLR doesn't allow us to inherit rules directly.
<cal_prelude>           [$space]+               ; -- WHITESPACE: skip
<cal_prelude>           (LineComment | BlockComment)
                                                ; -- COMMENT: skip
<cal_prelude>           "{"                     { (makeLexeme LbraceToken) `andBegin` cal_block }

-- Duplications of valid constant expression tokens that may appear in the
-- argument list.  This is an unfortunately large number of duplications.

-- Types.
<cal_block>             "qreg"                  { makeLexeme QregToken }
<cal_block>             "qubit"                 { makeLexeme QubitToken }
<cal_block>             "creg"                  { makeLexeme CregToken }
<cal_block>             "bool"                  { makeLexeme BoolToken }
<cal_block>             "bit"                   { makeLexeme BitToken }
<cal_block>             "int"                   { makeLexeme IntToken }
<cal_block>             "uint"                  { makeLexeme UintToken }
<cal_block>             "angle"                 { makeLexeme AngleToken }
<cal_block>             "float"                 { makeLexeme FloatToken }
<cal_block>             "array"                 { makeLexeme ArrayToken }
<cal_block>             "complex"               { makeLexeme ComplexToken }
<cal_block>             "duration"              { makeLexeme DurationToken }
-- Punctuation.
<cal_block>             "["                     { makeLexeme LbracketToken }
<cal_block>             "]"                     { makeLexeme RbracketToken }
<cal_block>             "("                     { makeLexeme LparenToken }
<cal_block>             ")"                     { makeLexeme RparenToken }
<cal_block>             "->"                    { makeLexeme ArrowToken }
<cal_block>             ","                     { makeLexeme CommaToken }
<cal_block>             "+"                     { makeLexeme PlusToken }
<cal_block>             "-"                     { makeLexeme MinusToken }
<cal_block>             "*"                     { makeLexeme AsteriskToken }
<cal_block>             "/"                     { makeLexeme SlashToken }
<cal_block>             ">>" | "<<"             { makeLexemeCat BitshiftOperatorToken }
-- Literals and names.
<cal_block>             "\"" ([01] "_"?)* [01] "\""
                                                { makeLexemeCat BitstringLiteralToken }
<cal_block>             ("0b" | "0B") ([01] "_"?)* [01]
                                                { makeLexemeCat BinaryIntegerLiteralToken }
<cal_block>             "0o" ([0-7] "_"?)* [0-7]
                                                { makeLexemeCat OctalIntegerLiteralToken }
<cal_block>             @decimalIntegerLiteral / ~$generalIdCharacter
                                                { makeLexemeCat DecimalIntegerLiteralToken }
<cal_block>             ("0x" | "0X") ([0-9a-fA-F] "_"?)* [0-9a-fA-F]
                                                { makeLexemeCat HexIntegerLiteralToken }
<cal_block>             @floatLiteral / ~$generalIdCharacter
                                                { makeLexemeCat FloatLiteralToken }

<cal_block>             "delay"                 { makeLexeme DelayToken }
<cal_block>             "reset"                 { makeLexeme ResetToken }
<cal_block>             "measure"               { makeLexeme MeasureToken }
<cal_block>             $firstIdCharacter $generalIdCharacter*
                                                { makeLexemeCat IdentifierToken }
<cal_block>             "$" [0-9]+              { makeLexemeCat HardwareQubitToken }

-- The meat-and-potatoes of matching a calibration block with balanced inner
-- braces.  We enter 'CAL_BLOCK' with the opening brace already tokenised
-- (that's how the lexer knew to swap modes to us), and with the token left open
-- to continue to accumulate.  We want to tokenise until we hit the balancing
-- brace.  Since we have _no_ knowledge of what the inner langauge is doing,
-- things like unbalanced braces in comments will cause a failure, but there's
-- not much we can do about that without greater spec restrictions.
<cal_block>             ~[\{\}]+                { makeLexemeCat CalibrationBlockToken }
<cal_block>             "{"                     { (makeLexeme LbraceToken) `andBegin` cal_block_2 }
<cal_block>             "}"                     { (makeLexeme RbraceToken) `andBegin` default_mode }

-- We define a max of 4 nested cal_block depths, ideally we would track the
-- nesting depth in an AlexUserState but I don't have time for that right now
<cal_block_2>           ~[\{\}]+                { makeLexemeCat CalibrationBlockToken }
<cal_block_2>           "{"                     { (makeLexeme LbraceToken) `andBegin` cal_block_3 }
<cal_block_2>           "}"                     { (makeLexeme RbraceToken) `andBegin` cal_block }
<cal_block_3>           ~[\{\}]+                { makeLexemeCat CalibrationBlockToken }
<cal_block_3>           "{"                     { (makeLexeme LbraceToken) `andBegin` cal_block_4 }
<cal_block_3>           "}"                     { (makeLexeme RbraceToken) `andBegin` cal_block_2 }
<cal_block_4>           ~[\{\}]+                { makeLexemeCat CalibrationBlockToken }
<cal_block_4>           "{"                     { (makeLexeme LbraceToken) `andBegin` cal_block_4 }
<cal_block_4>           "}"                     { (makeLexeme RbraceToken) `andBegin` cal_block_3 }

{
makeLexeme :: Token -> AlexInput -> Int -> Alex Lexeme
-- makeLexeme token (_, _, _, str) len | trace ("makeLexeme from \"" ++ (take len str) ++ "\"") False = undefined
makeLexeme token ((AlexPn _ l c), _, _, str) len = return (Lexeme (Just $ TextRef {moduleName="", sourceLine=l, sourceColumn=Just c}) token)

makeLexemeCat :: (String -> Token) -> AlexInput -> Int -> Alex Lexeme
-- makeLexemeCat mkToken (_, _, _, str) len | trace ("makeLexemeCat from \"" ++ (take len str) ++ "\"") False = undefined
makeLexemeCat mkToken ((AlexPn _ l c), _, _, str) len = return (Lexeme (Just $ TextRef {moduleName="", sourceLine=l, sourceColumn=Just c}) $ mkToken (take len str))

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
          (Lexeme _ EofToken) -> return lexemes
          (Lexeme _ _) -> do loop $! lexemes ++ [makeLexeme]
  loop []

alexEOF = return (Lexeme Nothing EofToken)

showPosn (AlexPn _ line col) = show line ++ ':' : show col
}
