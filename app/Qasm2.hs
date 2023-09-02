{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module Qasm2 where

import Ast qualified
import Control.Applicative
import Data.List (intercalate)

data AstNode tag context
  = AstNode {nodeTag :: tag, nodeChildren :: [AstNode tag context], nodeContext :: context}
  | AstNil

data Qasm2Tag
  -- Program
  = Program -- [version, stmts..]
  -- Statement
  | CregDecl -- [ident]
  | CregArrayDecl -- [ident, nnint]
  | QregDecl -- [ident]
  | QregArrayDecl -- [ident, nnint]
  | GateDecl -- ident (Maybe [ident]) [ident]
  | Opaque -- ident (Maybe [ident]) [ident]
  | If -- [ident, nnint, qop]
  | Uop -- [ident, IdNode (Maybe [ExpressionNode]) [ArgumentNode]
  | Barrier -- [arg..]
  | Measure -- [arg, arg]
  | Reset -- [arg]
  | List -- [elems..] -- elem is Identifier, IndexOp, or expression depending
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
  tryPretty (AstNode {nodeTag = Program, nodeChildren = version : stmts}) =
    tryPrettyPattern [S "OPENQASM ", N version, S ";\n\n", PPL "" stmts ";\n"]
  tryPretty (AstNode {nodeTag = CregDecl, nodeChildren = [ident]}) =
    tryPrettyPattern [S "creg ", N ident]
  tryPretty (AstNode {nodeTag = CregArrayDecl, nodeChildren = [ident, nnint]}) =
    tryPrettyPattern [S "creg ", N ident, S "[", N nnint, S "]"]
  tryPretty (AstNode {nodeTag = QregDecl, nodeChildren = [ident]}) =
    tryPrettyPattern [S "qreg ", N ident]
  tryPretty (AstNode {nodeTag = QregArrayDecl, nodeChildren = [ident, nnint]}) =
    tryPrettyPattern [S "qreg ", N ident, S "[", N nnint, S "]"]
  tryPretty (AstNode {nodeTag = GateDecl, nodeChildren = [ident, paramsList, qargsList]}) =
    tryPrettyPattern
      [ S "gate ",
        N ident,
        PILP "(" ", " (nodeChildren paramsList) ") ",
        PILP " " ", " (nodeChildren qargsList) ""
      ]
  tryPretty (AstNode {nodeTag = Opaque, nodeChildren = [ident, paramsList, qargsList]}) =
    tryPrettyPattern
      [ S "opaque ",
        N ident,
        PILP "(" ", " (nodeChildren paramsList) ")",
        PILP " " ", " (nodeChildren qargsList) ""
      ]
  tryPretty (AstNode {nodeTag = If, nodeChildren = [ident, nnint, qop]}) =
    tryPrettyPattern [S "if (", N ident, S " = ", N nnint, S ") ", N qop]
  tryPretty (AstNode {nodeTag = Uop, nodeChildren = [ident, params, qargs]}) =
    tryPrettyPattern [N ident, S "(", N params, S ") ", N qargs]
  tryPretty (AstNode {nodeTag = Barrier, nodeChildren = args}) =
    tryPrettyPattern [S "barrier ", IL ", " args]
  tryPretty (AstNode {nodeTag = Measure, nodeChildren = [qarg, carg]}) =
    tryPrettyPattern [N qarg, S " -> ", N carg]
  tryPretty (AstNode {nodeTag = Reset, nodeChildren = [arg]}) =
    tryPrettyPattern [S "reset ", N arg]
  tryPretty (AstNode {nodeTag = List, nodeChildren = elems}) =
    tryPrettyPattern [IL ", " elems]
  tryPretty (AstNode {nodeTag = Block, nodeChildren = stmts}) =
    tryPrettyPattern [S "{\n", PPL "  " stmts ";\n", S "}"]
  tryPretty (AstNode {nodeTag = Paren, nodeChildren = [expr]}) =
    tryPrettyPattern [S "(", N expr, S ")"]
  tryPretty (AstNode {nodeTag = (BinaryOp op), nodeChildren = [leftExpr, rightExpr]}) =
    tryPrettyPattern [S "(", N leftExpr, S op, N rightExpr, S ")"]
  tryPretty (AstNode {nodeTag = (UnaryOp op), nodeChildren = [expr]}) =
    tryPrettyPattern [S op, S "(", N expr, S ")"]
  tryPretty (AstNode {nodeTag = (FunctionOp op), nodeChildren = [expr]}) =
    tryPrettyPattern [S op, S "(", N expr, S ")"]
  tryPretty (AstNode {nodeTag = IndexOp, nodeChildren = [ident, nnint]}) =
    tryPrettyPattern [N ident, S "[", N nnint, S "]"]
  tryPretty (AstNode {nodeTag = (Identifier ident), nodeChildren = []}) = Just ident
  tryPretty (AstNode {nodeTag = (RealLiteral realLit), nodeChildren = []}) = Just realLit
  tryPretty (AstNode {nodeTag = (NnIntegerLiteral intLit), nodeChildren = []}) = Just intLit
  tryPretty (AstNode {nodeTag = (Keyword kw), nodeChildren = []}) = Just kw
  tryPretty (AstNode {}) = Nothing
  tryPretty AstNil = Just undefined

data (Ast.AstNode n) => PrettyPattern n
  = S String
  | N n
  | PPL String [n] String
  | IL String [n]
  | PILP String String [n] String

tryPrettyPattern :: [PrettyPattern (AstNode Qasm2Tag c)] -> Maybe String
tryPrettyPattern list =
  let tryElement (S str) = [Just str]
      tryElement (N node) = [Ast.tryPretty node]
      tryElement (PPL prefix list suffix) = (list >>= \e -> [Just prefix, Ast.tryPretty e, Just suffix])
      tryElement (IL _ []) = []
      tryElement (IL _ [n]) = [Ast.tryPretty n]
      tryElement (IL inter (n : tail)) = Ast.tryPretty n : Just inter : (tryElement $ IL inter tail)
      tryElement (PILP _ _ [] _) = []
      tryElement (PILP pre inter list post) = Just pre : tryElement (IL inter list) ++ [Just post]
   in concat <$> sequenceA (concatMap tryElement list)
