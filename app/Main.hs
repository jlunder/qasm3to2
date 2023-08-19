module Main where

import System.Environment (getArgs)

import Qasm3To2
import qualified Qasm2 as Q2
import qualified Qasm3 as Q3

q3Test = Q3.Program (Q3.Version $ Q3.VersionSpecifier "3.0") []

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (head args)
  print $ Q2.pretty $ Qasm3To2.toQasm2 q3Test

