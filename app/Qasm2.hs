{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module Qasm2 where

import Ast
import Data.List (intercalate)

data ProgramNode = Program RealNode [StatementNode]
  deriving (Eq, Read, Show)

data StatementNode
  = CregDeclStatement IdNode (Maybe NnIntegerNode)
  | QregDeclStatement IdNode (Maybe NnIntegerNode)
  | GateDeclStatement IdNode (Maybe [IdNode]) [IdNode] [GopNode]
  | OpaqueStatement IdNode (Maybe [IdNode]) [IdNode]
  | QopStatement QopNode
  | IfStatement IdNode NnIntegerNode QopNode
  deriving (Eq, Read, Show)

data QopNode
  = UopQop IdNode (Maybe [ExpressionNode]) [ArgumentNode]
  | BarrierQop [ArgumentNode]
  | MeasureQop ArgumentNode ArgumentNode
  | ResetQop ArgumentNode
  deriving (Eq, Read, Show)

data GopNode
  = UopGop IdNode (Maybe [ExpressionNode]) [ArgumentNode]
  | BarrierGop [IdNode]
  deriving (Eq, Read, Show)

data ArgumentNode = Scalar IdNode | Indexed IdNode NnIntegerNode
  deriving (Eq, Read, Show)

data ExpressionNode
  = RealLiteral RealNode
  | IntLiteral NnIntegerNode
  | ParenExpression ExpressionNode
  | UnaryExpression UnaryOperatorNode
  | BinaryExpression BinaryOperatorNode
  deriving (Eq, Read, Show)

--   :| exp + exp | exp - exp | exp * exp
--   :| exp / exp | -exp | exp ^ exp
--   :| "(" exp ")" | unaryop "(" exp ")"
data BinaryOperatorNode = BinaryOperator ExpressionNode String ExpressionNode
  deriving (Eq, Read, Show)

-- unaryop: "sin" | "cos" | "tan" | "exp" | "ln" | "sqrt"
data UnaryOperatorNode = UnaryOperator String ExpressionNode
  deriving (Eq, Read, Show)

-- id        := [a-z][A-Za-z0-9_]*
newtype IdNode = Id String
  deriving (Eq, Read, Show)

-- real      := ([0-9]+\.[0-9]*|[0-9]*\.[0-9]+)([eE][-+]?[0-9]+)?
newtype RealNode = Real String
  deriving (Eq, Read, Show)

-- nninteger := [1-9]+[0-9]*|0
newtype NnIntegerNode = NnInteger String
  deriving (Eq, Read, Show)

instance AstNode ProgramNode where
  pretty (Program qasmVersion statements) =
    "OPENQASM "
      ++ (pretty qasmVersion)
      ++ ";\n\n"
      ++ (concatMap (\stmt -> (pretty stmt) ++ "\n") statements)

instance AstNode StatementNode where
  pretty (CregDeclStatement ident optIndex) =
    "creg " ++ (pretty ident) ++ (prettyOptIndex optIndex) ++ ";"
  pretty (QregDeclStatement ident optIndex) =
    "qreg " ++ (pretty ident) ++ (prettyOptIndex optIndex) ++ ";"
  pretty (GateDeclStatement ident optDeclParams declBits declGops) =
    "gate "
      ++ (pretty ident)
      ++ (prettyOptDeclParams optDeclParams)
      ++ (prettyDeclBits declBits)
      ++ " {\n"
      ++ (concatMap pretty declGops)
      ++ "}\n"
  pretty (OpaqueStatement ident optDeclParams declBits) =
    "opaque " ++ (pretty ident) ++ (prettyOptDeclParams optDeclParams) ++ (prettyDeclBits declBits) ++ ";"
  pretty (QopStatement qop) =
    (pretty qop) ++ ";"
  pretty (IfStatement ident val qop) =
    "if (" ++ (pretty ident) ++ " == " ++ (pretty val) ++ ") " ++ (pretty qop) ++ ";"

instance AstNode QopNode where
  pretty (UopQop ident (Just exprs) []) = (pretty ident) ++ "(" ++ (prettyList exprs) ++ ")"
  pretty (UopQop ident (Just exprs) args) = (pretty ident) ++ "(" ++ (prettyList exprs) ++ ") " ++ (prettyList args)
  pretty (UopQop ident Nothing []) = (pretty ident)
  pretty (UopQop ident Nothing args) = (pretty ident) ++ " " ++ (prettyList args)
  pretty (BarrierQop args) = "barrier " ++ (prettyList args)
  pretty (MeasureQop arg1 arg2) = "measure " ++ (pretty arg1) ++ " " ++ (pretty arg2)
  pretty (ResetQop arg) = "reset " ++ (pretty arg)

instance AstNode GopNode where
  pretty (UopGop ident exprs args) = pretty (UopQop ident exprs args)
  pretty (BarrierGop idents) = pretty (BarrierQop $ map Scalar idents)

instance AstNode ArgumentNode where
  pretty (Scalar idNode) = pretty idNode
  pretty (Indexed idNode indexNode) = (pretty idNode) ++ "[" ++ (pretty indexNode) ++ "]"

instance AstNode ExpressionNode where
  pretty (RealLiteral node) = pretty node
  pretty (IntLiteral node) = pretty node
  pretty (ParenExpression node) = pretty node
  pretty (UnaryExpression node) = pretty node
  pretty (BinaryExpression node) = pretty node

instance AstNode BinaryOperatorNode where
  pretty (BinaryOperator exprA op exprB) = "(" ++ (pretty exprA) ++ " " ++ op ++ " " ++ (pretty exprB) ++ ")"

instance AstNode UnaryOperatorNode where
  pretty (UnaryOperator op expr) = "(" ++ op ++ " " ++ (pretty expr) ++ ")"

instance AstNode IdNode where
  pretty (Id ident) = ident

instance AstNode RealNode where
  pretty (Real r) = r

instance AstNode NnIntegerNode where
  pretty (NnInteger i) = i

prettyList :: (AstNode a) => [a] -> [Char]
prettyList idents = intercalate ", " $ map pretty idents

prettyOptIndex :: (Maybe NnIntegerNode) -> String
prettyOptIndex (Just index) = "[" ++ (pretty index) ++ "]"
prettyOptIndex Nothing = ""

prettyOptDeclParams :: (Maybe [IdNode]) -> String
prettyOptDeclParams (Just idents) = "(" ++ (prettyList idents) ++ ")"
prettyOptDeclParams Nothing = ""

prettyDeclBits :: [IdNode] -> String
prettyDeclBits [] = ""
prettyDeclBits bits = " " ++ (prettyList bits)

parse :: String -> Maybe ProgramNode
parse programText = Just (Program (Real "2") [])
