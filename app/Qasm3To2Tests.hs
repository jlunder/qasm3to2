{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fmap" #-}
module Main where

import Ast
import Control.Monad
import Debug.Trace (trace)
import Qasm2 qualified
-- import Qasm2Parser qualified
import Qasm3 qualified
import Qasm3Arbitrary qualified
import Qasm3Parser qualified
-- import Qasm3To2
import System.Exit (exitFailure)
import Test.HUnit
import Test.QuickCheck

instance (Qasm3Arbitrary.Normalizable b) => Qasm3Arbitrary.Normalizable (Either a b) where
  norm (Left err) = Left err
  norm (Right ast) = Right (Qasm3Arbitrary.norm ast)

tests =
  TestList
    [ TestLabel "test1" $ TestCase $ assertEqual "check" "1" (show 1)
    ]

prop_roundTrip = forAll Qasm3Arbitrary.arbitraryProgram $
  \ast -> Qasm3Arbitrary.norm (Right ast) == Qasm3Arbitrary.norm (Qasm3Parser.parseString (pretty ast))

main :: IO ()
main = do
  count <- runTestTT tests
  unless (failures count == 0) exitFailure

  result <- quickCheckResult prop_roundTrip
  unless (isSuccess result) exitFailure
