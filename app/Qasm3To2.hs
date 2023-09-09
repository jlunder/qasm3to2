{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Qasm3To2 where

import Ast
import Qasm2 (Tag (NnIntegerLiteral))
import Qasm2 qualified
import Qasm3 qualified

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

fromQasm3Statements :: Converter [Qasm3.SyntaxNode] -> [Qasm2.SyntaxNode]
fromQasm3Statements (Conversion []) = []
fromQasm3Statements (Conversion (stmt : stmts)) =
  let Conversion stmts2 = fromQasm3StatementAnnotation $ Conversion stmt
   in stmts2 ++ fromQasm3Statements (Conversion stmts)

fromQasm3StatementAnnotation :: Converter Qasm3.SyntaxNode -> Converter [Qasm2.SyntaxNode]
fromQasm3StatementAnnotation (Conversion (AstNode (Qasm3.Pragma param _) _ _)) = failConversion ""
fromQasm3StatementAnnotation (Conversion (AstNode Qasm3.Statement [stmt] _)) =
  fromQasm3StatementContent $ Conversion stmt
fromQasm3StatementAnnotation (Conversion (AstNode Qasm3.Statement _ _)) = failConversion ""

fromQasm3StatementContent :: Converter Qasm3.SyntaxNode -> Converter [Qasm2.SyntaxNode]
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.AliasDeclStmt [Identifier, Expression..] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.AssignmentStmt {assignmentOpTok :: Token} [IndexedIdentifier, (Expression | MeasureExpr)] _)) = failConversion ""
fromQasm3StatementContent c@(Conversion (AstNode Qasm3.BarrierStmt qbs _)) = Conversion [AstNode Qasm2.Barrier [] ()]
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.BoxStmt [time::Expression?, Scope] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.BreakStmt [] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.CalStmt {calBlockTok :: Token} [] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.DefcalgrammarStmt {calgrammarName :: String, calgrammarTok :: Token} [] _)) = failConversion ""

-- [ScalarType | ArrayType, Identifier, DeclarationExpr?]
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.ClassicalDeclStmt [t, ident, init] _)) = failConversion ""

-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.ConstDecl [ScalarType, Identifier, DeclarationExpr] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.ContinueStmt [] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.DefStmt [Identifier, List<ArgumentDefinition>, ScalarType?, Scope] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.DefcalStmt [DefcalTarget, List<(Expression | ArgumentDefinition)>?, List<HardwareQubit | Identifier>, ScalarType?, CalBlock] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.DelayStmt [Expression, (HardwareQubit | IndexedIdentifier)..] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.EndStmt [] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.ExpressionStmt [expr] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.ExternStmt [Identifier, List<ScalarType>, returnType::ScalarType?] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.ForStmt [ScalarType, Identifier, (Expression | Range | Set), (Statement | Scope)] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.GateStmt [Identifier, List<Identifier>?, List<Identifier>, Scope] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.GateCallStmt [modifiers::List<GateModifier>, target::Identifier, params::List<Expression>?, designator::Expression?, args::List<(HardwareQubit | IndexedIdentifier)>?] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.IfStmt [condition::Expression, thenBlock::(Statement | Scope), elseBlock::(Statement | Scope)?] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.IncludeStmt {includePath :: String, includeTok :: Token} [] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.InputIoDeclStmt [(ScalarType | ArrayType), Identifier] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.OutputIoDeclStmt [(ScalarType | ArrayType), Identifier] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.MeasureArrowAssignmentStmt [(HardwareQubit | IndexedIdentifier), IndexedIdentifier?] _)) = failConversion ""
-- [Identifier, designator::Expression?]
fromQasm3StatementContent c@(Conversion (AstNode Qasm3.CregOldStyleDeclStmt children _)) =
  case children of
    [AstNode (Qasm3.Identifier ident _) [] _, maybeDesgn] ->
      let q2Ident = AstNode (Qasm2.Identifier ident) [] ()
       in case maybeDesgn of
            NilNode -> Conversion [AstNode Qasm2.CregDecl [q2Ident] ()]
            expr@(AstNode {}) -> do
              q2Size <- return 1 -- evaluateNnint $ expr
              return [AstNode Qasm2.CregArrayDecl [q2Ident, AstNode (NnIntegerLiteral (show q2Size)) [] ()] ()]--mkNnIntegerLiteral q2Size] ()]
    -- _ -> failConversion "Invalid array "
    _ -> failConversion ("Invalid children of Qasm3.CregOldStyleDeclStmt: " ++ show children)
-- [Identifier, designator::Expression?]
fromQasm3StatementContent c@(Conversion (AstNode Qasm3.QregOldStyleDeclStmt [ident, maybeDesgn] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.QuantumDeclStmt [QubitType, Identifier] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.ResetStmt [(HardwareQubit | IndexedIdentifier)] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.ReturnStmt [(Expression | MeasureExpr)?] _)) = failConversion ""
-- fromQasm3StatementContent c@(Conversion (AstNode Qasm3.WhileStmt [Expression, (Statement | Scope)] _)) = failConversion ""
fromQasm3StatementContent (Conversion (AstNode t _ _)) = failConversion ("Unexpected statement content " ++ show t)

mkNnIntegerLiteral val = AstNode (NnIntegerLiteral $ show val) [] ()

fromQasm3 :: Qasm3.ParseNode -> Qasm2.SyntaxNode
fromQasm3 (AstNode (Qasm3.Program {}) qasm3Statements _) =
  let qasm3Analyzed = map Qasm3.syntaxTreeFrom qasm3Statements-- Qasm3.analyze qasm3Statements
   in AstNode
        Qasm2.Program
        (AstNode (Qasm2.RealLiteral "2") [] () : fromQasm3Statements (Conversion qasm3Analyzed))
        ()
