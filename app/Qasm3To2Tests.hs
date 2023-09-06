{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fmap" #-}
module Main where

import Ast
import Control.Monad
import Debug.Trace (trace)
import Qasm2 qualified
-- import Qasm2Parser qualified

-- import Qasm3To2

import Ast qualified
import Data.Either (fromRight)
import Qasm3 (tokenStringVal)
import Qasm3 qualified as Q3
import Qasm3Arbitrary qualified as Q3A
import Qasm3Parser qualified as Q3P
import System.Exit (exitFailure)
import Test.HUnit
import Test.QuickCheck

tests =
  TestList
    [ TestLabel "test1" $ TestCase $ assertEqual "check" "1" (show 1)
    ]

cfg = Q3A.defaultConfig

prop_roundTrip = forAll (Q3A.arbitraryProgramNode cfg) $
  \ast -> Ast.normAst ast == Ast.normAst (fromRight NilNode $ Q3P.parseString (Q3.pretty ast))

main :: IO ()
main = do
  -- count <- runTestTT tests
  -- unless (failures count == 0) exitFailure

  result <- quickCheckResult (withMaxSuccess 1 prop_roundTrip)
  --unless (isSuccess result) exitFailure
