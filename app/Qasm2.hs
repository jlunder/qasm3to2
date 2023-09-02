{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module Qasm2 where

import Ast (pretty)
import Ast qualified
import Control.Applicative
import Data.List (intercalate)

data AstNode tag context
  = AstNode {nodeTag :: tag, nodeChildren :: [AstNode tag context], nodeContext :: context}
  | AstNil

data Qasm2Tag
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

instance Ast.AstNode (AstNode Qasm2Tag context) where
  pretty (AstNode {nodeTag = Program, nodeChildren = version : stmts}) =
    "OPENQASM " ++ pretty version ++ ";\n\n" ++ concatMap ((++ ";\n") . pretty) stmts
  pretty (AstNode {nodeTag = CregDecl, nodeChildren = [ident]}) =
    "creg " ++ pretty ident
  pretty (AstNode {nodeTag = CregArrayDecl, nodeChildren = [ident, nnint]}) =
    "creg " ++ pretty ident ++ "[" ++ pretty nnint ++ "]"
  pretty (AstNode {nodeTag = QregDecl, nodeChildren = [ident]}) =
    "qreg " ++ pretty ident
  pretty (AstNode {nodeTag = QregArrayDecl, nodeChildren = [ident, nnint]}) =
    "qreg " ++ pretty ident ++ "[" ++ pretty nnint ++ "]"
  pretty (AstNode {nodeTag = GateDecl, nodeChildren = [ident, paramsList, qargsList]}) =
    "gate "
      ++ pretty ident
      ++ (pilp "(" " ++ " (nodeChildren paramsList) ") ")
      ++ (pilp " " ", " (nodeChildren qargsList) "")
  pretty (AstNode {nodeTag = Opaque, nodeChildren = [ident, paramsList, qargsList]}) =
    "opaque "
      ++ pretty ident
      ++ pilp "(" ", " (nodeChildren paramsList) ")"
      ++ pilp " " ", " (nodeChildren qargsList) ""
  pretty (AstNode {nodeTag = If, nodeChildren = [ident, nnint, qop]}) =
    "if (" ++ pretty ident ++ " = " ++ pretty nnint ++ ") " ++ pretty qop
  pretty (AstNode {nodeTag = Uop, nodeChildren = [ident, params, qargs]}) =
    pretty ident ++ "(" ++ pretty params ++ ") " ++ pretty qargs
  pretty (AstNode {nodeTag = Barrier, nodeChildren = args}) =
    "barrier " ++ il ", " args
  pretty (AstNode {nodeTag = Measure, nodeChildren = [qarg, carg]}) =
    pretty qarg ++ " -> " ++ pretty carg
  pretty (AstNode {nodeTag = Reset, nodeChildren = [arg]}) =
    "reset " ++ pretty arg
  pretty (AstNode {nodeTag = List, nodeChildren = elems}) =
    il ", " elems
  pretty (AstNode {nodeTag = Block, nodeChildren = stmts}) =
    "{\n" ++ concatMap (("  " ++) . (++ ";\n") . pretty) stmts ++ "}"
  pretty (AstNode {nodeTag = Paren, nodeChildren = [expr]}) =
    "(" ++ pretty expr ++ ")"
  pretty (AstNode {nodeTag = (BinaryOp op), nodeChildren = [leftExpr, rightExpr]}) =
    "(" ++ pretty leftExpr ++ op ++ pretty rightExpr ++ ")"
  pretty (AstNode {nodeTag = (UnaryOp op), nodeChildren = [expr]}) =
    op ++ "(" ++ pretty expr ++ ")"
  pretty (AstNode {nodeTag = (FunctionOp op), nodeChildren = [expr]}) =
    op ++ "(" ++ pretty expr ++ ")"
  pretty (AstNode {nodeTag = IndexOp, nodeChildren = [ident, nnint]}) =
    pretty ident ++ "[" ++ pretty nnint ++ "]"
  pretty (AstNode {nodeTag = (Identifier ident), nodeChildren = []}) = ident
  pretty (AstNode {nodeTag = (RealLiteral realLit), nodeChildren = []}) = realLit
  pretty (AstNode {nodeTag = (NnIntegerLiteral intLit), nodeChildren = []}) = intLit
  pretty (AstNode {nodeTag = (Keyword kw), nodeChildren = []}) = kw
  pretty (AstNode {}) = undefined
  pretty AstNil = undefined

il _ [] = []
il _ [n] = pretty n
il inter (n : tail) = pretty n ++ inter ++ (il inter tail)

pilp _ _ [] _ = ""
pilp pre inter list post = pre ++ (il inter list) ++ post
