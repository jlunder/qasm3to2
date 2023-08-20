module Main where

-- import Ast
import Control.Monad (when)
-- import Qasm2 qualified as Qasm2
-- import Qasm3 qualified as Qasm3
-- import Qasm3To2
import System.Exit (exitFailure)
import Test.HUnit

-- import Test.QuickCheck

tests =
  TestList
    [ TestLabel "test1" $ TestCase $ assertEqual "check" "1" (show 1)
    ]

-- propRoundTripQasm2 :: Qasm2.ProgramNode -> Bool
-- propRoundTripQasm2 ast = (Just ast) == Qasm2.parse (pretty ast)

-- propRoundTripQasm3 :: Qasm3.ProgramNode -> Bool
-- propRoundTripQasm3 ast = (Just ast) == Qasm3.parse (pretty ast)

main :: IO ()
main = do
  count <- runTestTT tests
  when (failures count > 0) exitFailure

-- quickCheck propRoundTrip
