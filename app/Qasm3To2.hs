{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Qasm3To2 where

import Ast qualified
import Qasm2 (Tag (NnIntegerLiteral))
import Qasm2 qualified
import Qasm3.SemanticGraph qualified
import Qasm3.Syntax qualified

fromQasm3 :: Qasm3.SemanticGraph.SemanticGraph -> Qasm2.SyntaxNode
fromQasm3
  ( Qasm3.SemanticGraph.SemanticGraph
      { Qasm3.SemanticGraph.semGraphProgram = Qasm3.SemanticGraph.Program qasm3Statements
      }
    ) =
    Ast.Node
      Qasm2.Program
      (Ast.Node (Qasm2.RealLiteral "2") [] () : map fromQasm3Statement qasm3Statements)
      ()

fromQasm3Statement s = Ast.NilNode