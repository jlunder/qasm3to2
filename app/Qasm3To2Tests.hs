module Main where

import Control.Monad (when)
import Qasm3To2
import System.Exit (exitFailure)
import Test.HUnit

tests =
  TestList
    [ TestLabel "test1" $ TestCase $ assertEqual "check" "1" (show 1)
    ]

main :: IO ()
main = do
  count <- runTestTT tests
  when (failures count > 0) exitFailure
