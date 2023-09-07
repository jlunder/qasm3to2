{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Qasm3
  ( ParseNode,
    SyntaxNode,
    Token (..),
    Tag (..),
    pretty,
    syntaxTreeFrom,
    tokenIdentifierName,
    tokenIntegerVal,
    tokenFloatVal,
    tokenBooleanVal,
    tokenBitstringVal,
    tokenTimingVal,
    tokenHwQubitIndex,
    tokenVersionMajMin,
    tokenStringVal,
    tokenStr,
  )
where

import Ast
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Debug.Trace (trace)

type ParseNode = AstNode Tag SourceRef

type SyntaxNode = AstNode Tag ()

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
  | -- EqualityOperator
    DoubleEqualsToken
  | ExclamationPointEqualsToken
  | -- CompoundAssignmentOperator
    PlusEqualsToken
  | MinusEqualsToken
  | AsteriskEqualsToken
  | SlashEqualsToken
  | AmpersandEqualsToken
  | PipeEqualsToken
  | TildeEqualsToken
  | CaretEqualsToken
  | DoubleLessEqualsToken
  | DoubleGreaterEqualsToken
  | PercentEqualsToken
  | DoubleAsteriskEqualsToken
  | -- ComparisonOperator
    LessToken
  | GreaterToken
  | LessEqualsToken
  | GreaterEqualsToken
  | -- BitshiftOperator
    DoubleLessToken
  | DoubleGreaterToken
  | --
    ImaginaryLiteralToken String
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

data Timing
  = TimeDt Double
  | TimeNs Double
  | TimeUs Double
  | TimeMs Double
  | TimeS Double
  deriving (Eq, Read, Show)

data Tag
  = Program {openqasmMajorVersion :: Int, openqasmMinorVersion :: Maybe Int, versionTok :: Token} -- [Statement..]
  | Pragma {pragmaContent :: String, pragmaTok :: Token} -- []
  | Statement -- [<StatementContent>, Annotation..]
  | Annotation {annotationName :: String, annotationContent :: String, annotationTok :: Token} -- []
  | Scope -- [Statement..]
  -- <StatementContent>
  | AliasDecl -- [Identifier, Expression..]
  | Assignment {assignmentOpTok :: Token} -- [IndexedIdentifier, (Expression | MeasureExpr)]
  | Barrier -- [(HardwareQubit | IndexedIdentifier)..]
  | Box -- [time::Expression?, Scope]
  | Break -- []
  | Cal {calBlockTok :: Token} -- []
  | Defcalgrammar {calgrammarName :: String, calgrammarTok :: Token} -- []
  | ClassicalDecl -- [ScalarType | ArrayType, Identifier, DeclarationExpr?]
  | ConstDecl -- [ScalarType, Identifier, DeclarationExpr]
  | Continue -- []
  | Def -- [Identifier, List<ArgumentDefinition>, ScalarType?, Scope]
  | Defcal -- [DefcalTarget, List<(Expression | ArgumentDefinition)>?, List<HardwareQubit | Identifier>, ScalarType?, CalBlock]
  | Delay -- [Expression, (HardwareQubit | IndexedIdentifier)..]
  | End -- []
  | ExpressionStmt -- [expr]
  | Extern -- [Identifier, List<ScalarType>, returnType::ScalarType?]
  | For -- [ScalarType, Identifier, (Expression | Range | Set), (Statement | Scope)]
  | Gate -- [Identifier, List<Identifier>?, List<Identifier>, Scope]
  | GateCall -- [modifiers::List<GateModifier>, target::Identifier, params::List<Expression>?, designator::Expression?, args::List<(HardwareQubit | IndexedIdentifier)>?]
  | If -- [condition::Expression, thenBlock::(Statement | Scope), elseBlock::(Statement | Scope)?
  | Include {includePath :: String, includeTok :: Token} -- []
  | InputIoDecl -- [(ScalarType | ArrayType), Identifier]
  | OutputIoDecl -- [(ScalarType | ArrayType), Identifier]
  | MeasureArrowAssignment -- [(HardwareQubit | IndexedIdentifier), IndexedIdentifier?]
  | CregOldStyleDecl -- [Identifier, designator::Expression?]
  | QregOldStyleDecl -- [Identifier, designator::Expression?]
  | QuantumDecl -- [QubitType, Identifier]
  | Reset -- [(HardwareQubit | IndexedIdentifier)]
  | Return -- [(Expression | MeasureExpr)?]
  | While -- [Expression, (Statement | Scope)]
  -- <Expression>
  | ParenExpr -- [Expression]
  | IndexExpr -- [Expression, (List<RangeExpr | Expression> | SetExpr)]
  | UnaryOperatorExpr {unaryOp :: Token} -- [Expression]
  | BinaryOperatorExpr {binaryOp :: Token} -- [left::Expression, right::Expression]
  | CastExpr -- [(ScalarType | ArrayType), Expression]
  | DurationOfExpr -- [Scope]
  | CallExpr -- [Identifier, ExpressionNode]
  --   Array only allowed in array initializers
  | ArrayExpr -- [elements::Expression..]
  --   Set, Range only allowed in (some) indexing expressions
  | SetExpr -- [elements::Expression..]
  | RangeExpr -- [begin::Expression?, step::Expression?, end::Expression?]
  --   Dim only allowed in (some) array arg definitions
  | DimExpr -- []
  | MeasureExpr -- [expr]
  | Identifier {identifierName :: String, identifierTok :: Token} -- []
  | IntegerLiteral {integerVal :: Integer, integerTok :: Token} -- []
  | FloatLiteral {floatVal :: Double, floatTok :: Token} -- []
  | ImaginaryLiteral {imaginaryVal :: Double, imaginaryTok :: Token} -- []
  | BooleanLiteral {booleanVal :: Bool, booleanTok :: Token} -- []
  | BitstringLiteral {bitstringVal :: [Bool], bitstringTok :: Token} -- []
  | TimingLiteral {timingVal :: Timing, timingTok :: Token} -- []
  | HardwareQubit {hwQubitIndex :: Int, hwQubitTok :: Token} -- []
  --
  | IndexedIdentifier -- [Identifier, List<RangeExpr | Expression> | SetExpr>..]
  -- <GateModifier>
  | InvGateModifier -- []
  | PowGateModifier -- [Expression]
  | CtrlGateModifier -- [Expression?]
  | NegCtrlGateModifier -- [Expression?]
  -- <ScalarType>
  | BitType -- [size::Expression?]
  | IntType -- [size::Expression?]
  | UintType -- [size::Expression?]
  | FloatType -- [size::Expression?]
  | AngleType -- [size::Expression?]
  | BoolType
  | DurationType
  | StretchType
  | ComplexType -- [base::ScalarType?]
  | CregType -- [size::Expression?]
  | QregType -- [size::Expression?]
  -- Special types
  | QubitType -- [size::Expression?]
  | ArrayType -- [base::ScalarType, indices::Expression..]
  -- <ArrayReferenceType>
  | ReadonlyArrayRefType -- [base::ScalarType, sizes::(DimExpr | List<Expression>)]
  | MutableArrayRefType -- [base::ScalarType, sizes::(DimExpr | List<Expression>)]
  --
  | DefcalTarget {defcalTargetName :: String, defcalTargetTok :: Token} -- []
  | ArgumentDefinition -- [{Scalar,Qubit,Creg,Qreg,*ArrayRef}Type, Identifier]
  | List -- [element..]
  deriving (Eq, Read, Show)

pretty :: (Show c) => AstNode Tag c -> String
pretty (AstNode (Program _ _ tok) stmts _) =
  "OPENQASM " ++ tokenStr tok ++ ";\n\n" ++ concatMap ((++ "\n") . pretty) stmts
pretty (AstNode (Pragma ctnt _) [] _) = "pragma " ++ ctnt
pretty (AstNode Statement (stmt : annots) _) = concatMap ((++ "\n") . pretty) annots ++ pretty stmt
pretty (AstNode (Annotation name ctnt _) [] _) = '@' : name ++ " " ++ ctnt
pretty (AstNode Scope stmts _) = "{\n" ++ concatMap ((++ "\n") . pretty) stmts ++ "}\n"
pretty (AstNode AliasDecl (ident : exprs) _) =
  "let " ++ pretty ident ++ " = " ++ intercalate " ++ " (map pretty exprs) ++ ";"
pretty (AstNode (Assignment op) [target, expr] _) = pretty target ++ " " ++ tokenStr op ++ " " ++ pretty expr ++ ";"
pretty (AstNode Barrier gateOperands _) = "barrier " ++ prettyListElements gateOperands ++ ";"
pretty (AstNode Box [time, stmts] _) = "box" ++ prettyMaybeDsgn time ++ " " ++ prettyBlock stmts
pretty (AstNode Break [] _) = "break;"
pretty (AstNode (Cal calBlock) [] _) = "cal " ++ tokenStr calBlock
pretty (AstNode (Defcalgrammar _ cgname) [] _) = "defcalgrammar \"" ++ tokenStr cgname ++ "\";"
pretty (AstNode ClassicalDecl [anyType, ident, maybeExpr] _) =
  pretty anyType ++ " " ++ pretty ident ++ prettyMaybe " = " maybeExpr "" ++ ";"
pretty (AstNode ConstDecl [sclrType, ident, maybeExpr] _) =
  pretty sclrType ++ " " ++ pretty ident ++ prettyMaybe " = " maybeExpr "" ++ ";"
pretty (AstNode Continue [] _) = "continue;"
pretty (AstNode Def [ident, argDefs, returnType, stmts] _) =
  "def "
    ++ pretty ident
    ++ "("
    ++ prettyList argDefs
    ++ ")"
    ++ prettyReturnType returnType
    ++ prettyBlock stmts
pretty (AstNode Delay (designator : gateOperands) _) = "delay" ++ pretty designator ++ " " ++ prettyListElements gateOperands ++ ";"
pretty (AstNode Defcal [defcalTarget, defcalArgs, defcalOps, returnType, calBlock] _) =
  "defcal "
    ++ pretty defcalTarget
    ++ (if isNilNode defcalArgs then " " else "(" ++ prettyList defcalArgs ++ ") ")
    ++ prettyList defcalOps
    ++ prettyReturnType returnType
    ++ " "
    ++ pretty calBlock
pretty (AstNode End [] _) = "end;"
pretty (AstNode ExpressionStmt [expr] _) = pretty expr ++ ";"
pretty (AstNode Extern [ident, paramTypes, returnType] _) =
  -- paramTypes are scalar, arrayRef, or CREG
  "extern " ++ pretty ident ++ "(" ++ prettyList paramTypes ++ ")" ++ prettyReturnType returnType ++ ";"
pretty (AstNode For [anyType, ident, loopExpr, loopStmt] _) =
  "for " ++ pretty anyType ++ " " ++ pretty ident ++ " in " ++ pretty loopExpr ++ " " ++ pretty loopStmt
pretty (AstNode Gate [ident, params, args, stmts] _) =
  pretty ident
    ++ (if isNilNode params then "" else "(" ++ prettyList params ++ ")")
    ++ (if isNilNode args then "" else ' ' : prettyList args)
    ++ pretty stmts
pretty (AstNode GateCall [modifiers, target, params, maybeTime, gateArgs] _) =
  concatMap ((++ " ") . pretty) (astChildren modifiers)
    ++ pretty target
    ++ prettyMaybeList "(" params ")"
    ++ prettyMaybe "[" maybeTime "]"
    ++ prettyMaybeList " " gateArgs ""
    ++ ";"
pretty (AstNode If [condExpr, thenBlock, maybeElseBlock] _) =
  "if (" ++ pretty condExpr ++ ") " ++ pretty thenBlock ++ prettyMaybe " else " maybeElseBlock ""
pretty (AstNode (Include _ tok) [] _) = "include " ++ tokenStr tok ++ ";"
pretty (AstNode InputIoDecl [anyType, ident] _) = "input " ++ pretty anyType ++ " " ++ pretty ident ++ ";"
pretty (AstNode OutputIoDecl [anyType, ident] _) = "output " ++ pretty anyType ++ " " ++ pretty ident ++ ";"
pretty (AstNode MeasureArrowAssignment [msrExpr, maybeTgt] _) =
  pretty msrExpr ++ prettyMaybe " -> " maybeTgt "" ++ ";"
pretty (AstNode CregOldStyleDecl [ident, maybeSize] _) = "creg " ++ pretty ident ++ prettyMaybeDsgn maybeSize ++ ";"
pretty (AstNode QregOldStyleDecl [ident, maybeSize] _) = "qreg " ++ pretty ident ++ prettyMaybeDsgn maybeSize ++ ";"
pretty (AstNode QuantumDecl [qubitType, ident] _) = pretty qubitType ++ " " ++ pretty ident ++ ";"
pretty (AstNode Reset [gateOp] _) = "reset " ++ pretty gateOp ++ ";"
pretty (AstNode Return [maybeExpr] _) = "return" ++ prettyMaybe " " maybeExpr "" ++ ";"
pretty (AstNode While [condExpr, loopBlock] _) = "while (" ++ pretty condExpr ++ ") " ++ pretty loopBlock
pretty (AstNode ParenExpr [expr] _) = "(" ++ pretty expr ++ ")"
pretty (AstNode IndexExpr [expr, index] _) = "(" ++ pretty expr ++ ")[" ++ pretty index ++ "]"
pretty (AstNode (UnaryOperatorExpr op) [expr] _) = tokenStr op ++ "(" ++ pretty expr ++ ")"
pretty (AstNode (BinaryOperatorExpr op) [left, right] _) =
  "(" ++ pretty left ++ ") " ++ tokenStr op ++ " (" ++ pretty right ++ ")"
pretty (AstNode CastExpr [anyType, expr] _) = pretty anyType ++ "(" ++ pretty expr ++ ")"
pretty (AstNode DurationOfExpr stmts _) = "durationof( {\n" ++ concatMap ((++ "\n") . pretty) stmts ++ "} )"
pretty (AstNode CallExpr (ident : exprs) _) = pretty ident ++ "(" ++ prettyListElements exprs ++ ")"
pretty (AstNode (Identifier _ tok) [] _) = tokenStr tok
pretty (AstNode (IntegerLiteral _ tok) [] _) = tokenStr tok
pretty (AstNode (FloatLiteral _ tok) [] _) = tokenStr tok
pretty (AstNode (ImaginaryLiteral _ tok) [] _) = tokenStr tok
pretty (AstNode (BooleanLiteral _ tok) [] _) = tokenStr tok
pretty (AstNode (BitstringLiteral _ tok) [] _) = tokenStr tok
pretty (AstNode (TimingLiteral _ tok) [] _) = tokenStr tok
pretty (AstNode (HardwareQubit _ tok) [] _) = tokenStr tok
pretty (AstNode ArrayExpr elems _) = "{" ++ prettyListElements elems ++ "}"
pretty (AstNode SetExpr elems _) = "{" ++ prettyListElements elems ++ "}"
pretty (AstNode RangeExpr [begin, step, end] _) =
  prettyMaybe "" begin "" ++ ":" ++ prettyMaybe "" step ":" ++ prettyMaybe "" end ""
pretty (AstNode DimExpr [size] _) = "#dim=" ++ pretty size
pretty (AstNode MeasureExpr [gateOp] _) = "measure " ++ pretty gateOp
pretty (AstNode IndexedIdentifier (ident : indices) _) =
  pretty ident ++ concatMap (\idx -> "[" ++ prettyIndex idx ++ "]") indices
pretty (AstNode InvGateModifier [] _) = "inv at"
pretty (AstNode PowGateModifier [expr] _) = "pow(" ++ pretty expr ++ ")"
pretty (AstNode CtrlGateModifier [maybeExpr] _) = "ctrl " ++ prettyMaybe "(" maybeExpr ") " ++ "at"
pretty (AstNode NegCtrlGateModifier [maybeExpr] _) = "negctrl " ++ prettyMaybe "(" maybeExpr ") " ++ "at"
pretty (AstNode BitType [maybeSize] _) = "bit" ++ prettyMaybeDsgn maybeSize
pretty (AstNode CregType [maybeSize] _) = "creg" ++ prettyMaybeDsgn maybeSize
pretty (AstNode QregType [maybeSize] _) = "qreg" ++ prettyMaybeDsgn maybeSize
pretty (AstNode IntType [maybeSize] _) = "int" ++ prettyMaybeDsgn maybeSize
pretty (AstNode UintType [maybeSize] _) = "uint" ++ prettyMaybeDsgn maybeSize
pretty (AstNode FloatType [maybeSize] _) = "float" ++ prettyMaybeDsgn maybeSize
pretty (AstNode AngleType [maybeSize] _) = "angle" ++ prettyMaybeDsgn maybeSize
pretty (AstNode BoolType [] _) = "bool"
pretty (AstNode DurationType [] _) = "duration"
pretty (AstNode StretchType [] _) = "stretch"
pretty (AstNode ComplexType [maybeSclr] _) = "complex" ++ prettyMaybeDsgn maybeSclr
pretty (AstNode QubitType [maybeSize] _) = "qubit" ++ prettyMaybeDsgn maybeSize
pretty (AstNode ArrayType (sclrType : exprs) _) =
  "array[" ++ pretty sclrType ++ ", " ++ prettyListElements exprs ++ "]"
pretty (AstNode ReadonlyArrayRefType (sclrType : exprs) _) =
  "readonly array[" ++ pretty sclrType ++ ", " ++ prettyListElements exprs ++ "]"
pretty (AstNode MutableArrayRefType (sclrType : exprs) _) =
  "mutable array[" ++ pretty sclrType ++ ", " ++ prettyListElements exprs ++ "]"
pretty (AstNode (DefcalTarget tgt _) [] _) = tgt -- "measure", "reset", "delay", or some other identifier
-- does not handle CREG, QREG args (postfix size designator)
pretty (AstNode ArgumentDefinition [anyType, ident] _) = pretty anyType ++ " " ++ pretty ident
{- Error cases -}
-- Should have been handled above -- usually implies some change to how the surrounding renders
pretty NilNode = trace "Unhandled NilNode for pretty" undefined
-- Should have been handled above -- we can't know which separator to use
pretty (AstNode List elems _) = trace ("Unhandled List node for pretty with children: " ++ show elems) undefined
-- Fallback
pretty node = trace ("Missing pattern for pretty: " ++ show node) undefined


-- The syntax tree is as close to canonicalized as the tree easily gets
syntaxTreeFrom :: AstNode Tag c -> SyntaxNode
syntaxTreeFrom NilNode = NilNode
syntaxTreeFrom (AstNode ParenExpr [expr] _) = syntaxTreeFrom expr
syntaxTreeFrom (AstNode ParenExpr children _) = undefined
syntaxTreeFrom (AstNode tag children _) = AstNode tag (map syntaxTreeFrom children) ()


-- Utility functions

tokenIdentifierName :: Token -> String
tokenIdentifierName (IdentifierToken str) = str
tokenIdentifierName (AnnotationKeywordToken ('@' : str)) = str
tokenIdentifierName (AnnotationKeywordToken str) = str

tokenIntegerVal :: Token -> Integer
tokenIntegerVal (BinaryIntegerLiteralToken str) = 7040 -- str
tokenIntegerVal (OctalIntegerLiteralToken str) = 7040 -- str
tokenIntegerVal (DecimalIntegerLiteralToken str) = 7040 -- str
tokenIntegerVal (HexIntegerLiteralToken str) = 7040 -- str

tokenFloatVal :: Token -> Double
tokenFloatVal (FloatLiteralToken str) = 7040.0 -- str
tokenFloatVal (ImaginaryLiteralToken str) = 7040.0 -- str

tokenBooleanVal :: Token -> Bool
tokenBooleanVal (BooleanLiteralToken str) = str == "true"

tokenBitstringVal :: Token -> [Bool]
tokenBitstringVal (BitstringLiteralToken str) = [] -- str

tokenTimingVal :: Token -> Timing
tokenTimingVal (TimingLiteralToken str) = TimeDt 7040.0 -- str

tokenHwQubitIndex :: Token -> Int
tokenHwQubitIndex (HardwareQubitToken str) = 7040 -- str

tokenVersionMajMin :: Token -> (Int, Maybe Int)
tokenVersionMajMin (VersionSpecifierToken str) =
  let split sep [] = ([], Nothing)
      split sep (x : xs) =
        if x == sep
          then ([], Just xs)
          else let (foundHead, foundTail) = split sep xs in (x : foundHead, foundTail)
      (major, minor) = split ',' str
      majorVal = read major
      minorVal = case minor of Nothing -> Nothing; Just "" -> Nothing; Just minorStr -> Just (read minorStr)
   in (majorVal, minorVal)

tokenStringVal :: Token -> String
tokenStringVal (StringLiteralToken ('"' : strTail)) =
  case reverse strTail of
    '"' : strMid -> reverse strMid
    _ -> strTail
tokenStringVal (StringLiteralToken str) = str
tokenStringVal (RemainingLineContentToken str) = str
tokenStringVal (CalibrationBlockToken str) = str

tokenStr :: Token -> String
tokenStr OpenqasmToken = "OPENQASM"
tokenStr IncludeToken = "include"
tokenStr DefcalgrammarToken = "defcalgrammar"
tokenStr DefToken = "def"
tokenStr CalToken = "cal"
tokenStr DefcalToken = "defcal"
tokenStr GateToken = "gate"
tokenStr ExternToken = "extern"
tokenStr BoxToken = "box"
tokenStr LetToken = "let"
tokenStr BreakToken = "break"
tokenStr ContinueToken = "continue"
tokenStr IfToken = "if"
tokenStr ElseToken = "else"
tokenStr EndToken = "end"
tokenStr ReturnToken = "return"
tokenStr ForToken = "for"
tokenStr WhileToken = "while"
tokenStr InToken = "in"
tokenStr PragmaToken = "#pragma"
tokenStr (AnnotationKeywordToken kw) = kw
tokenStr InputToken = "input"
tokenStr OutputToken = "output"
tokenStr ConstToken = "const"
tokenStr ReadonlyToken = "readonly"
tokenStr MutableToken = "mutable"
tokenStr QregToken = "qreg"
tokenStr QubitToken = "qubit"
tokenStr CregToken = "creg"
tokenStr BoolToken = "bool"
tokenStr BitToken = "bit"
tokenStr IntToken = "int"
tokenStr UintToken = "uint"
tokenStr FloatToken = "float"
tokenStr AngleToken = "angle"
tokenStr ComplexToken = "complex"
tokenStr ArrayToken = "array"
tokenStr VoidToken = "void"
tokenStr DurationToken = "duration"
tokenStr StretchToken = "stretch"
tokenStr GphaseToken = "gphase"
tokenStr InvToken = "inv"
tokenStr PowToken = "pow"
tokenStr CtrlToken = "ctrl"
tokenStr NegctrlToken = "negctrl"
tokenStr DimToken = "dim"
tokenStr DurationofToken = "durationof"
tokenStr DelayToken = "delay"
tokenStr ResetToken = "reset"
tokenStr MeasureToken = "measure"
tokenStr BarrierToken = "barrier"
tokenStr (BooleanLiteralToken str) = str
tokenStr LbracketToken = "["
tokenStr RbracketToken = "]"
tokenStr LbraceToken = "{"
tokenStr RbraceToken = "}"
tokenStr LparenToken = "("
tokenStr RparenToken = ")"
tokenStr ColonToken = ":"
tokenStr SemicolonToken = ";"
tokenStr DotToken = "."
tokenStr CommaToken = ","
tokenStr EqualsToken = "="
tokenStr ArrowToken = "->"
tokenStr PlusToken = "+"
tokenStr DoublePlusToken = "++"
tokenStr MinusToken = "-"
tokenStr AsteriskToken = "*"
tokenStr DoubleAsteriskToken = "**"
tokenStr SlashToken = "/"
tokenStr PercentToken = "%"
tokenStr PipeToken = "|"
tokenStr DoublePipeToken = "||"
tokenStr AmpersandToken = "&"
tokenStr DoubleAmpersandToken = "&&"
tokenStr CaretToken = "^"
tokenStr AtToken = "@"
tokenStr TildeToken = "~"
tokenStr ExclamationPointToken = "!"
tokenStr DoubleEqualsToken = "=="
tokenStr ExclamationPointEqualsToken = "!="
tokenStr PlusEqualsToken = "+="
tokenStr MinusEqualsToken = "-="
tokenStr AsteriskEqualsToken = "*="
tokenStr SlashEqualsToken = "/="
tokenStr AmpersandEqualsToken = "&="
tokenStr PipeEqualsToken = "|="
tokenStr TildeEqualsToken = "~="
tokenStr CaretEqualsToken = "^="
tokenStr DoubleLessEqualsToken = "<<="
tokenStr DoubleGreaterEqualsToken = ">>="
tokenStr PercentEqualsToken = "%="
tokenStr DoubleAsteriskEqualsToken = "**="
tokenStr LessToken = "<"
tokenStr GreaterToken = ">"
tokenStr LessEqualsToken = "<="
tokenStr GreaterEqualsToken = ">="
tokenStr DoubleLessToken = "<<"
tokenStr DoubleGreaterToken = ">>"
tokenStr (ImaginaryLiteralToken str) = str
tokenStr (BinaryIntegerLiteralToken str) = str
tokenStr (OctalIntegerLiteralToken str) = str
tokenStr (DecimalIntegerLiteralToken str) = str
tokenStr (HexIntegerLiteralToken str) = str
tokenStr (IdentifierToken str) = str
tokenStr (HardwareQubitToken str) = str
tokenStr (FloatLiteralToken str) = str
tokenStr (TimingLiteralToken str) = str
tokenStr (BitstringLiteralToken str) = str
tokenStr (WhitespaceToken str) = str
tokenStr NewlineToken = "\n"
tokenStr (LineCommentToken str) = str
tokenStr (BlockCommentToken str) = str
tokenStr (VersionSpecifierToken str) = str
tokenStr (StringLiteralToken str) = str
tokenStr (RemainingLineContentToken str) = str
tokenStr (CalibrationBlockToken str) = str

prettyTiming :: Timing -> String
prettyTiming (TimeDt t) = show t ++ "dt"
prettyTiming (TimeNs t) = show t ++ "ns"
prettyTiming (TimeUs t) = show t ++ "us"
prettyTiming (TimeMs t) = show t ++ "ms"
prettyTiming (TimeS t) = show t ++ "s"

prettyBlock :: (Show c) => AstNode Tag c -> String
prettyBlock NilNode = ""
prettyBlock (AstNode List stmts _) = "{\n" ++ concatMap ((++ "\n") . pretty) stmts ++ "}"

prettyIndex :: (Show c) => AstNode Tag c -> String
prettyIndex idx = if astTag idx == List then prettyList idx else pretty idx

prettyList :: (Show c) => AstNode Tag c -> String
prettyList NilNode = ""
prettyList (AstNode List elems _) = prettyListElements elems

prettyMaybeDsgn :: (Show c) => AstNode Tag c -> String
prettyMaybeDsgn expr = prettyMaybe "[" expr "]"

prettyMaybeList :: (Show c) => String -> AstNode Tag c -> String -> String
prettyMaybeList _ NilNode _ = ""
prettyMaybeList pre (AstNode List elems _) post = pre ++ prettyListElements elems ++ post

prettyMaybe :: (Show c) => String -> AstNode Tag c -> String -> String
prettyMaybe _ NilNode _ = ""
prettyMaybe pre expr post = pre ++ pretty expr ++ post

prettyListElements :: (Show c) => [AstNode Tag c] -> String
prettyListElements elems = intercalate ", " (map pretty elems)

prettyReturnType :: (Show c) => AstNode Tag c -> String
prettyReturnType NilNode = ""
prettyReturnType returnType = " -> " ++ pretty returnType

indent :: String -> String
indent block = concatMap (\s -> "  " ++ s ++ "\n") $ lines block
