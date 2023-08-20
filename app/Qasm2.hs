{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module Qasm2 where

import Data.List (intercalate)

data ProgramNode = Program RealNode [StatementNode]

data StatementNode
  = CregDeclStatement IdNode (Maybe NnIntegerNode)
  | QregDeclStatement IdNode (Maybe NnIntegerNode)
  | GateDeclStatement IdNode (Maybe [IdNode]) [IdNode] [GopNode]
  | OpaqueStatement IdNode (Maybe [IdNode]) [IdNode]
  | QopStatement QopNode
  | IfStatement IdNode NnIntegerNode QopNode

data QopNode
  = UopQop IdNode (Maybe [ExpressionNode]) [ArgumentNode]
  | BarrierQop [ArgumentNode]
  | MeasureQop ArgumentNode ArgumentNode
  | ResetQop ArgumentNode

data GopNode
  = UopGop IdNode (Maybe [ExpressionNode]) [ArgumentNode]
  | BarrierGop [IdNode]

data ArgumentNode = Scalar IdNode | Indexed IdNode NnIntegerNode

data ExpressionNode
  = RealLiteral RealNode
  | IntLiteral NnIntegerNode
  | ParenExpression ExpressionNode
  | UnaryExpression UnaryOperatorNode
  | BinaryExpression BinaryOperatorNode

--   :| exp + exp | exp - exp | exp * exp
--   :| exp / exp | -exp | exp ^ exp
--   :| "(" exp ")" | unaryop "(" exp ")"
data BinaryOperatorNode = BinaryOperator ExpressionNode String ExpressionNode

-- unaryop: "sin" | "cos" | "tan" | "exp" | "ln" | "sqrt"
data UnaryOperatorNode = UnaryOperator String ExpressionNode

-- id        := [a-z][A-Za-z0-9_]*
newtype IdNode = Id String

-- real      := ([0-9]+\.[0-9]*|[0-9]*\.[0-9]+)([eE][-+]?[0-9]+)?
newtype RealNode = Real String

-- nninteger := [1-9]+[0-9]*|0
newtype NnIntegerNode = NnInteger String

class AstNode a where
  pretty :: a -> String

instance AstNode ProgramNode where
  pretty :: ProgramNode -> String
  pretty (Program qasmVersion statements) =
    "OPENQASM "
      ++ (pretty qasmVersion)
      ++ ";\n\n"
      ++ (concatMap (\stmt -> (pretty stmt) ++ "\n") statements)

instance AstNode StatementNode where
  pretty :: StatementNode -> String
  pretty (CregDeclStatement id optIndex) =
    "creg " ++ (pretty id) ++ (prettyOptIndex optIndex) ++ ";"
  pretty (QregDeclStatement id optIndex) =
    "qreg " ++ (pretty id) ++ (prettyOptIndex optIndex) ++ ";"
  pretty (GateDeclStatement id optDeclParams declBits declGops) =
    "gate "
      ++ (pretty id)
      ++ (prettyOptDeclParams optDeclParams)
      ++ (prettyDeclBits declBits)
      ++ " {\n"
      ++ (concatMap pretty declGops)
      ++ "}\n"
  pretty (OpaqueStatement id optDeclParams declBits) =
    "opaque " ++ (pretty id) ++ (prettyOptDeclParams optDeclParams) ++ (prettyDeclBits declBits) ++ ";"
  pretty (QopStatement qop) =
    (pretty qop) ++ ";"
  pretty (IfStatement id val qop) =
    "if (" ++ (pretty id) ++ " == " ++ (pretty val) ++ ") " ++ (pretty qop) ++ ";"

instance AstNode QopNode where
  pretty :: QopNode -> String
  pretty (UopQop id (Just exprs) []) = (pretty id) ++ "(" ++ (prettyList exprs) ++ ")"
  pretty (UopQop id (Just exprs) args) = (pretty id) ++ "(" ++ (prettyList exprs) ++ ") " ++ (prettyList args)
  pretty (UopQop id Nothing []) = (pretty id)
  pretty (UopQop id Nothing args) = (pretty id) ++ " " ++ (prettyList args)
  pretty (BarrierQop args) = "barrier " ++ (prettyList args)
  pretty (MeasureQop arg1 arg2) = "measure " ++ (pretty arg1) ++ " " ++ (pretty arg2)
  pretty (ResetQop arg) = "reset " ++ (pretty arg)

instance AstNode GopNode where
  pretty :: GopNode -> String
  pretty (UopGop id exprs args) = pretty (UopQop id exprs args)
  pretty (BarrierGop ids) = pretty (BarrierQop $ map Scalar ids)

instance AstNode ArgumentNode where
  pretty :: ArgumentNode -> String
  pretty (Scalar idNode) = pretty idNode
  pretty (Indexed idNode indexNode) = (pretty idNode) ++ "[" ++ (pretty indexNode) ++ "]"

instance AstNode ExpressionNode where
  pretty :: ExpressionNode -> String
  pretty (RealLiteral node) = pretty node
  pretty (IntLiteral node) = pretty node
  pretty (ParenExpression node) = pretty node
  pretty (UnaryExpression node) = pretty node
  pretty (BinaryExpression node) = pretty node

instance AstNode BinaryOperatorNode where
  pretty :: BinaryOperatorNode -> String
  pretty (BinaryOperator exprA op exprB) = "(" ++ (pretty exprA) ++ " " ++ op ++ " " ++ (pretty exprB) ++ ")"

instance AstNode UnaryOperatorNode where
  pretty :: UnaryOperatorNode -> String
  pretty (UnaryOperator op expr) = "(" ++ op ++ " " ++ (pretty expr) ++ ")"

instance AstNode IdNode where
  pretty :: IdNode -> String
  pretty (Id id) = id

instance AstNode RealNode where
  pretty :: RealNode -> String
  pretty (Real r) = r

instance AstNode NnIntegerNode where
  pretty :: NnIntegerNode -> String
  pretty (NnInteger i) = i

prettyList :: AstNode a => [a] -> [Char]
prettyList ids = intercalate ", " $ map pretty ids

prettyOptIndex :: (Maybe NnIntegerNode) -> String
prettyOptIndex (Just index) = "[" ++ (pretty index) ++ "]"
prettyOptIndex Nothing = ""

prettyOptDeclParams :: (Maybe [IdNode]) -> String
prettyOptDeclParams (Just ids) = "(" ++ (prettyList ids) ++ ")"
prettyOptDeclParams Nothing = ""

prettyDeclBits :: [IdNode] -> String
prettyDeclBits [] = ""
prettyDeclBits bits = " " ++ (prettyList bits)
