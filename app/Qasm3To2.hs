{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Qasm3To2 where

import Ast
import Qasm2 (Tag (NnIntegerLiteral))
import Qasm2 qualified
import Qasm3.Syntax qualified as Q3S

-- Needed: expression interpreter to eval const expressions
-- Needed: statement interpreter to eval loops, functions, etc.
-- Needed: internal representation of QASM2-compatible constructs

data Converter a where
  Conversion :: {payload :: a} -> Converter a
  Failure :: {errorMessage :: String} -> Converter a
  deriving (Eq, Show)

instance Functor Converter where
  fmap :: (a -> b) -> Converter a -> Converter b
  fmap f c@Conversion {payload = x} = c {payload = f x}
  fmap f (Failure a) = Failure a

instance Applicative Converter where
  (<*>) :: Converter (a -> b) -> Converter a -> Converter b
  c@(Conversion {payload = x}) <*> Conversion {payload = y} = c {payload = x y}
  Conversion _ <*> Failure y = Failure y
  Failure x <*> _ = Failure x

  pure :: a -> Converter a
  pure x = Conversion {payload = x}

instance Monad Converter where
  (>>=) :: Converter a -> (a -> Converter b) -> Converter b
  c@Conversion {payload = a} >>= f = c {payload = payload (f a)}

failConversion :: String -> Converter a
failConversion err = Failure {errorMessage = err}

fromQasm3Statements :: Converter [Q3S.SyntaxNode] -> [Qasm2.SyntaxNode]
fromQasm3Statements (Conversion []) = []
fromQasm3Statements (Conversion (stmt : stmts)) =
  let Conversion stmts2 = fromQasm3StatementAnnotation $ Conversion stmt
   in stmts2 ++ fromQasm3Statements (Conversion stmts)

fromQasm3StatementAnnotation :: Converter Q3S.SyntaxNode -> Converter [Qasm2.SyntaxNode]
fromQasm3StatementAnnotation (Conversion (AstNode (Q3S.Pragma param _) _ _)) = failConversion ""
fromQasm3StatementAnnotation (Conversion (AstNode Q3S.Statement [stmt] _)) =
  fromQasm3StatementContent $ Conversion stmt
fromQasm3StatementAnnotation (Conversion (AstNode Q3S.Statement _ _)) = failConversion ""

fromQasm3StatementContent :: Converter Q3S.SyntaxNode -> Converter [Qasm2.SyntaxNode]
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.AliasDeclStmt [Identifier, Expression..] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.AssignmentStmt {assignmentOpTok :: Token} [IndexedIdentifier, (Expression | MeasureExpr)] _)) = failConversion ""
fromQasm3StatementContent c@(Conversion (AstNode Q3S.BarrierStmt qbs _)) = Conversion [AstNode Qasm2.Barrier [] ()]
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.BoxStmt [time::Expression?, Scope] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.BreakStmt [] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.CalStmt {calBlockTok :: Token} [] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.DefcalgrammarStmt {calgrammarName :: String, calgrammarTok :: Token} [] _)) = failConversion ""

-- [ScalarType | ArrayType, Identifier, DeclarationExpr?]
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.ClassicalDeclStmt [t, ident, init] _)) = failConversion ""

-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.ConstDecl [ScalarType, Identifier, DeclarationExpr] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.ContinueStmt [] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.DefStmt [Identifier, List<ArgumentDefinition>, ScalarType?, Scope] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.DefcalStmt [DefcalTarget, List<(Expression | ArgumentDefinition)>?, List<HardwareQubit | Identifier>, ScalarType?, CalBlock] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.DelayStmt [Expression, (HardwareQubit | IndexedIdentifier)..] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.EndStmt [] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.ExpressionStmt [expr] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.ExternStmt [Identifier, List<ScalarType>, returnType::ScalarType?] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.ForStmt [ScalarType, Identifier, (Expression | Range | Set), (Statement | Scope)] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.GateStmt [Identifier, List<Identifier>?, List<Identifier>, Scope] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.GateCallStmt [modifiers::List<GateModifier>, target::Identifier, params::List<Expression>?, designator::Expression?, args::List<(HardwareQubit | IndexedIdentifier)>?] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.IfStmt [condition::Expression, thenBlock::(Statement | Scope), elseBlock::(Statement | Scope)?] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.IncludeStmt {includePath :: String, includeTok :: Token} [] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.InputIoDeclStmt [(ScalarType | ArrayType), Identifier] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.OutputIoDeclStmt [(ScalarType | ArrayType), Identifier] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.MeasureArrowAssignmentStmt [(HardwareQubit | IndexedIdentifier), IndexedIdentifier?] _)) = failConversion ""
-- [Identifier, designator::Expression?]
fromQasm3StatementContent c@(Conversion (AstNode Q3S.CregOldStyleDeclStmt children _)) =
  case children of
    [AstNode (Q3S.Identifier ident _) [] _, maybeDesgn] ->
      let q2Ident = AstNode (Qasm2.Identifier ident) [] ()
       in case maybeDesgn of
            NilNode -> Conversion [AstNode Qasm2.CregDecl [q2Ident] ()]
            expr@(AstNode {}) -> do
              q2Size <- return 1 -- evaluateNnint $ expr
              return [AstNode Qasm2.CregArrayDecl [q2Ident, AstNode (NnIntegerLiteral (show q2Size)) [] ()] ()]--mkNnIntegerLiteral q2Size] ()]
    -- _ -> failConversion "Invalid array "
    _ -> failConversion ("Invalid children of Q3S.CregOldStyleDeclStmt: " ++ show children)
-- [Identifier, designator::Expression?]
fromQasm3StatementContent c@(Conversion (AstNode Q3S.QregOldStyleDeclStmt [ident, maybeDesgn] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.QuantumDeclStmt [QubitType, Identifier] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.ResetStmt [(HardwareQubit | IndexedIdentifier)] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.ReturnStmt [(Expression | MeasureExpr)?] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Q3S.WhileStmt [Expression, (Statement | Scope)] _)) = failConversion ""
fromQasm3StatementContent (Conversion (AstNode t _ _)) = failConversion ("Unexpected statement content " ++ show t)

mkNnIntegerLiteral :: Show a => a -> AstNode Tag ()
mkNnIntegerLiteral val = AstNode (NnIntegerLiteral $ show val) [] ()

fromQasm3 :: Q3S.ParseNode -> Qasm2.SyntaxNode
fromQasm3 (AstNode (Q3S.Program {}) qasm3Statements _) =
  let qasm3Analyzed = map Q3S.syntaxTreeFrom qasm3Statements-- Q3S.analyze qasm3Statements
   in AstNode
        Qasm2.Program
        (AstNode (Qasm2.RealLiteral "2") [] () : fromQasm3Statements (Conversion qasm3Analyzed))
        ()
