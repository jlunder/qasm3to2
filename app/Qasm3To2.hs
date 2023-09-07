{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Qasm3To2 where

import Ast
import Data.Map
import Qasm2 qualified
import Qasm3 qualified

-- Needed: expression interpreter to eval const expressions
-- Needed: statement interpreter to eval loops, functions, etc.
-- Needed: internal representation of QASM2-compatible constructs

failConversion err = Failure {errorMessage = err}

data Converter a
  = Conversion
      { payload :: a
      }
  | Failure {errorMessage :: String}
  deriving (Eq, Show)

instance Functor Converter where
  fmap f (Conversion {payload = x}) = Conversion (f x)
  fmap f (Failure a) = Failure a

instance Applicative Converter where
  Conversion {payload = x} <*> Conversion {payload = y} = Conversion (x y)
  Conversion _ <*> Failure y = Failure y
  Failure x <*> _ = Failure x
  pure x = Conversion {payload = x}

instance Monad Converter where
  Conversion {payload = a} >>= f = f a

type Qasm2Ast = AstNode Qasm2.Tag ()

type Qasm3Ast = AstNode Qasm3.Tag ()

toQasm2Statements :: Converter [AstNode Qasm3.Tag c] -> [Qasm2Ast]
toQasm2Statements Conversion {payload = []} = []
toQasm2Statements Conversion {payload = (stmt : tail)} =
  let Conversion stmt2 = toQasm2Statement $ Conversion stmt
   in stmt2 : (toQasm2Statements $ Conversion {payload = tail})

toQasm2Statement :: Converter (AstNode Qasm3.Tag c) -> Converter Qasm2Ast
toQasm2Statement (Conversion (AstNode (Qasm3.Pragma param _) _ _)) = failConversion ""
toQasm2Statement (Conversion (AstNode Qasm3.Statement _ _)) = Conversion $ AstNode Qasm2.Barrier [] ()
--toQasm2Statement (Conversion (AstNode Qasm3.Statement (NilNode : annotations) _)) = failConversion ""

toQasm2 :: AstNode Qasm3.Tag c -> Qasm2Ast
toQasm2 (AstNode (Qasm3.Program {}) qasm3Statements _) =
  AstNode Qasm2.Program (toQasm2Statements $ Conversion qasm3Statements) ()
