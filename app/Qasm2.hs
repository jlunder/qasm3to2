{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module Qasm2 where

import Ast qualified
import Control.Applicative
import Data.List (intercalate)

type ParseNode = Ast.Node Tag Ast.SourceRef

type SyntaxNode = Ast.Node Tag ()

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

-- Convert the syntax tree back into a string form that can be parsed into an
-- equivalent tree
pretty :: (Eq c, Read c, Show c) => Ast.Node Tag c -> String
pretty (Ast.Node {Ast.tag = Program, Ast.children = version : stmts}) =
  "OPENQASM " ++ pretty version ++ ";\n\n" ++ concatMap ((++ ";\n") . pretty) stmts
pretty (Ast.Node {Ast.tag = CregDecl, Ast.children = [ident]}) =
  "creg " ++ pretty ident
pretty (Ast.Node {Ast.tag = CregArrayDecl, Ast.children = [ident, nnint]}) =
  "creg " ++ pretty ident ++ "[" ++ pretty nnint ++ "]"
pretty (Ast.Node {Ast.tag = QregDecl, Ast.children = [ident]}) =
  "qreg " ++ pretty ident
pretty (Ast.Node {Ast.tag = QregArrayDecl, Ast.children = [ident, nnint]}) =
  "qreg " ++ pretty ident ++ "[" ++ pretty nnint ++ "]"
pretty (Ast.Node {Ast.tag = GateDecl, Ast.children = [ident, paramsList, qargsList]}) =
  "gate "
    ++ pretty ident
    ++ (pilp "(" " ++ " (Ast.children paramsList) ") ")
    ++ (pilp " " ", " (Ast.children qargsList) "")
pretty (Ast.Node {Ast.tag = Opaque, Ast.children = [ident, paramsList, qargsList]}) =
  "opaque "
    ++ pretty ident
    ++ pilp "(" ", " (Ast.children paramsList) ")"
    ++ pilp " " ", " (Ast.children qargsList) ""
pretty (Ast.Node {Ast.tag = If, Ast.children = [ident, nnint, qop]}) =
  "if (" ++ pretty ident ++ " = " ++ pretty nnint ++ ") " ++ pretty qop
pretty (Ast.Node {Ast.tag = Uop, Ast.children = [ident, params, qargs]}) =
  pretty ident ++ "(" ++ pretty params ++ ") " ++ pretty qargs
pretty (Ast.Node {Ast.tag = Barrier, Ast.children = args}) =
  "barrier " ++ il ", " args
pretty (Ast.Node {Ast.tag = Measure, Ast.children = [qarg, carg]}) =
  pretty qarg ++ " -> " ++ pretty carg
pretty (Ast.Node {Ast.tag = Reset, Ast.children = [arg]}) =
  "reset " ++ pretty arg
pretty (Ast.Node {Ast.tag = List, Ast.children = elems}) =
  il ", " elems
pretty (Ast.Node {Ast.tag = Block, Ast.children = stmts}) =
  "{\n" ++ concatMap (("  " ++) . (++ ";\n") . pretty) stmts ++ "}"
pretty (Ast.Node {Ast.tag = Paren, Ast.children = [expr]}) =
  "(" ++ pretty expr ++ ")"
pretty (Ast.Node {Ast.tag = (BinaryOp op), Ast.children = [leftExpr, rightExpr]}) =
  "(" ++ pretty leftExpr ++ op ++ pretty rightExpr ++ ")"
pretty (Ast.Node {Ast.tag = (UnaryOp op), Ast.children = [expr]}) =
  op ++ "(" ++ pretty expr ++ ")"
pretty (Ast.Node {Ast.tag = (FunctionOp op), Ast.children = [expr]}) =
  op ++ "(" ++ pretty expr ++ ")"
pretty (Ast.Node {Ast.tag = IndexOp, Ast.children = [ident, nnint]}) =
  pretty ident ++ "[" ++ pretty nnint ++ "]"
pretty (Ast.Node {Ast.tag = (Identifier ident), Ast.children = []}) = ident
pretty (Ast.Node {Ast.tag = (RealLiteral realLit), Ast.children = []}) = realLit
pretty (Ast.Node {Ast.tag = (NnIntegerLiteral intLit), Ast.children = []}) = intLit
pretty (Ast.Node {Ast.tag = (Keyword kw), Ast.children = []}) = kw
pretty (Ast.Node {}) = undefined
pretty Ast.NilNode = undefined

il _ [] = []
il _ [n] = pretty n
il inter (n : tail) = pretty n ++ inter ++ (il inter tail)

pilp _ _ [] _ = ""
pilp pre inter list post = pre ++ (il inter list) ++ post
