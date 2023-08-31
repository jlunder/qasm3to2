{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fmap" #-}
module Main where

import Ast
import Control.Monad
import Debug.Trace (trace)
import Qasm2 qualified
-- import Qasm2Parser qualified
import Qasm3 qualified as Q3
import Qasm3Arbitrary qualified as Q3A
import Qasm3Parser qualified as Q3P
-- import Qasm3To2
import System.Exit (exitFailure)
import Test.HUnit
import Test.QuickCheck

instance (Q3A.Normalizable b) => Q3A.Normalizable (Either a b) where
  norm (Left err) = Left err
  norm (Right ast) = Right (Q3A.norm ast)

tests =
  TestList
    [ TestLabel "test1" $ TestCase $ assertEqual "check" "1" (show 1)
    ]

cfg = Q3A.defaultConfig

prop_roundTrip = forAll (Q3A.arbitraryProgramNode cfg) $
  \ast -> Q3A.norm (Right ast) == Q3A.norm (Q3P.parseString (pretty ast))

main :: IO ()
main = do
  count <- runTestTT tests
  unless (failures count == 0) exitFailure

  result <- quickCheckResult prop_roundTrip
  unless (isSuccess result) exitFailure
