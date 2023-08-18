module Main where

import System.Environment (getArgs)

import Qasm3To2 ( readExpr, typeInfer )

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (head args)
  let printTypes [] = return ()
      printTypes (line : lines) = do
        case line of
          '-' : '-' : _ -> putStrLn line -- Print out comment lines verbatim
          _ -> putStrLn $ typeInfer $ readExpr line
        printTypes lines
  printTypes (lines content)
