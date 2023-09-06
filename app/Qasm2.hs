{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module Qasm2 where

import Ast
import Control.Applicative
import Data.List (intercalate)

data Tag
  = -- Program
    Program -- [openqasm, version]
    -- Statement
  | CregDecl -- [ident]
  | CregArrayDecl -- [ident, nnint]
  | QregDecl -- [ident]
  | QregArrayDecl -- [ident, nnint]
  | GateDecl -- ident (Maybe [ident]) [ident]
  | Opaque -- ident (Maybe [ident]) [ident]
  | If -- [ident, nnint, qop]
  | Uop -- [ident, IdNode (Maybe [ExpressionNode]) [ArgumentNode]
  | Barrier -- [arg...]
  | Measure -- [arg, arg]
  | Reset -- [arg]
  | List -- elems -- elem is Identifier, IndexOp, or expression depending
  -- Block
  | Block -- stmts -- stmt is Uop, Barrier, or If, and no indices on args!
  -- Expression
  | Paren -- [expr]
  -- op = keyword ('+' | '-' | '*' | '/' | '^')
  | BinaryOp String -- [expr, expr]
  -- op = keyword ('-')
  | UnaryOp String -- [expr]
  -- op = keyword ("sin" | "cos" | "tan" | "exp" | "ln" | "sqrt")
  | FunctionOp String -- [expr]
  -- [a-z][A-Za-z0-9_]*
  | IndexOp -- [ident, nnint]
  | Identifier String -- []
  -- ([0-9]+\.[0-9]*|[0-9]*\.[0-9]+)([eE][-+]?[0-9]+)?
  | RealLiteral String -- []
  -- [1-9]+[0-9]*|0
  | NnIntegerLiteral String -- []
  | Keyword String -- []
  deriving (Eq, Read, Show)

pretty :: (Eq c, Read c, Show c) => AstNode Tag c -> String
pretty (AstNode {astTag = Program, astChildren = version : stmts}) =
  "OPENQASM " ++ pretty version ++ ";\n\n" ++ concatMap ((++ ";\n") . pretty) stmts
pretty (AstNode {astTag = CregDecl, astChildren = [ident]}) =
  "creg " ++ pretty ident
pretty (AstNode {astTag = CregArrayDecl, astChildren = [ident, nnint]}) =
  "creg " ++ pretty ident ++ "[" ++ pretty nnint ++ "]"
pretty (AstNode {astTag = QregDecl, astChildren = [ident]}) =
  "qreg " ++ pretty ident
pretty (AstNode {astTag = QregArrayDecl, astChildren = [ident, nnint]}) =
  "qreg " ++ pretty ident ++ "[" ++ pretty nnint ++ "]"
pretty (AstNode {astTag = GateDecl, astChildren = [ident, paramsList, qargsList]}) =
  "gate "
    ++ pretty ident
    ++ (pilp "(" " ++ " (astChildren paramsList) ") ")
    ++ (pilp " " ", " (astChildren qargsList) "")
pretty (AstNode {astTag = Opaque, astChildren = [ident, paramsList, qargsList]}) =
  "opaque "
    ++ pretty ident
    ++ pilp "(" ", " (astChildren paramsList) ")"
    ++ pilp " " ", " (astChildren qargsList) ""
pretty (AstNode {astTag = If, astChildren = [ident, nnint, qop]}) =
  "if (" ++ pretty ident ++ " = " ++ pretty nnint ++ ") " ++ pretty qop
pretty (AstNode {astTag = Uop, astChildren = [ident, params, qargs]}) =
  pretty ident ++ "(" ++ pretty params ++ ") " ++ pretty qargs
pretty (AstNode {astTag = Barrier, astChildren = args}) =
  "barrier " ++ il ", " args
pretty (AstNode {astTag = Measure, astChildren = [qarg, carg]}) =
  pretty qarg ++ " -> " ++ pretty carg
pretty (AstNode {astTag = Reset, astChildren = [arg]}) =
  "reset " ++ pretty arg
pretty (AstNode {astTag = List, astChildren = elems}) =
  il ", " elems
pretty (AstNode {astTag = Block, astChildren = stmts}) =
  "{\n" ++ concatMap (("  " ++) . (++ ";\n") . pretty) stmts ++ "}"
pretty (AstNode {astTag = Paren, astChildren = [expr]}) =
  "(" ++ pretty expr ++ ")"
pretty (AstNode {astTag = (BinaryOp op), astChildren = [leftExpr, rightExpr]}) =
  "(" ++ pretty leftExpr ++ op ++ pretty rightExpr ++ ")"
pretty (AstNode {astTag = (UnaryOp op), astChildren = [expr]}) =
  op ++ "(" ++ pretty expr ++ ")"
pretty (AstNode {astTag = (FunctionOp op), astChildren = [expr]}) =
  op ++ "(" ++ pretty expr ++ ")"
pretty (AstNode {astTag = IndexOp, astChildren = [ident, nnint]}) =
  pretty ident ++ "[" ++ pretty nnint ++ "]"
pretty (AstNode {astTag = (Identifier ident), astChildren = []}) = ident
pretty (AstNode {astTag = (RealLiteral realLit), astChildren = []}) = realLit
pretty (AstNode {astTag = (NnIntegerLiteral intLit), astChildren = []}) = intLit
pretty (AstNode {astTag = (Keyword kw), astChildren = []}) = kw
pretty (AstNode {}) = undefined
pretty NilNode = undefined

il _ [] = []
il _ [n] = pretty n
il inter (n : tail) = pretty n ++ inter ++ (il inter tail)

pilp _ _ [] _ = ""
pilp pre inter list post = pre ++ (il inter list) ++ post
