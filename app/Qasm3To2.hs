{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Qasm3To2 where

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
    fmap f (Conversion {payload=x}) = Conversion (f x)
    fmap f (Failure a) = Failure a

instance Applicative Converter where
  Conversion {payload = x} <*> Conversion {payload = y} = Conversion (x y)
  Conversion _ <*> Failure y = Failure y
  Failure x <*> _ = Failure x
  pure x = Conversion {payload = x}

instance Monad Converter where
    Conversion {payload = a} >>= f = f a

toQasm2Statements :: Converter [Qasm3.StatementNode] -> [Qasm2.AstNode Qasm2.Qasm2Tag ()]
toQasm2Statements Conversion {payload=[]} = []
toQasm2Statements Conversion {payload=(stmt : tail)} =
  let Conversion stmt2 = toQasm2Statement $ Conversion stmt
   in stmt2 : (toQasm2Statements $ Conversion {payload=tail})

toQasm2Statement :: Converter Qasm3.StatementNode -> Converter (Qasm2.AstNode Qasm2.Qasm2Tag ())
toQasm2Statement (Conversion (Qasm3.Pragma pragma param)) = failConversion ""
toQasm2Statement (Conversion (Qasm3.Annotated [] stmt)) = Conversion $ Qasm2.AstNode Qasm2.Barrier [] ()
toQasm2Statement (Conversion (Qasm3.Annotated annotations _)) = failConversion ""

toQasm2 :: Qasm3.ProgramNode -> Qasm2.AstNode Qasm2.Qasm2Tag ()
toQasm2 (Qasm3.Program _ _ qasm3Statements) =
  Qasm2.AstNode Qasm2.Program (toQasm2Statements $ Conversion qasm3Statements) ()
