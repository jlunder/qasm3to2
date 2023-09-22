{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Qasm3To2 where

import Ast qualified
import Chatty
import Qasm2 (Tag (NnIntegerLiteral))
import Qasm2 qualified
import Qasm3.Result
import Qasm3.SemanticGraph (Expression (..), Program (..), SemanticGraph (..), Statement (..))
import Qasm3.Syntax qualified

fromQasm3 :: SemanticGraph -> Result Qasm2.SyntaxNode
fromQasm3 (SemanticGraph {semGraphProgram = Program qasm3Statements}) = do
  newStmts <- sequence $ concatMap (sequence . fromQasm3Statement) qasm3Statements
  return $ Ast.Node Qasm2.Program (Ast.Node (Qasm2.RealLiteral "2") [] () : newStmts) ()

fromQasm3Statement :: Statement -> Result [Qasm2.SyntaxNode]
-- fromQasm3Statement (AliasDeclarationStmt {}) = undefined
fromQasm3Statement (ConstDeclarationStmt exprType ident valEpxr) = undefined -- TODO
fromQasm3Statement (ClassicalDeclarationStmt ioMod exprType ident initExpr) = undefined -- TODO
fromQasm3Statement (QuantumDeclarationStmt exprType ident) = undefined -- TODO
fromQasm3Statement (AssignmentStmt tgtExpr valExpr) = undefined -- TODO
-- fromQasm3Statement (CompoundAssignmentStmt {}) = undefined
fromQasm3Statement (GateCallStmt ident params args) = undefined -- TODO
-- fromQasm3Statement (ResetStmt expr) = undefined
-- fromQasm3Statement (BarrierStmt {}) = undefined
-- fromQasm3Statement (DelayStmt {}) = undefined
-- fromQasm3Statement (BoxStmt {}) = undefined
-- fromQasm3Statement BreakStmt = undefined
-- fromQasm3Statement ContinueStmt = undefined
-- fromQasm3Statement EndStmt = undefined
-- fromQasm3Statement (ReturnStmt {}) = undefined
-- fromQasm3Statement (ExpressionStmt {}) = undefined
fromQasm3Statement (IfStmt expr [stmt] []) = undefined -- TODO
-- fromQasm3Statement (IfStmt {}) = undefined
-- fromQasm3Statement (ForStmt {}) = undefined
-- fromQasm3Statement (WhileStmt {}) = undefined
fromQasm3Statement stmt = return []
