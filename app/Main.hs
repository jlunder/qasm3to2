module Main where

import Ast qualified
import Control.Monad
import Data.Either
import Qasm2 qualified as Q2
import Qasm3.SemanticAnalyzer qualified as Q3A
import Qasm3.SemanticGraph qualified as Q3G
import Qasm3.Syntax qualified as Q3S
import Qasm3.Lexer qualified as Q3L
import Qasm3.Parser qualified as Q3P
import Qasm3To2
import System.Console.GetOpt
import System.Directory
import System.Environment

data Flag
  = Reject
  deriving (Show, Eq)

options :: [OptDescr Flag]
options =
  []

execOpts :: IO ([Flag], [String])
execOpts =
  do
    argv <- getArgs
    progName <- getProgName
    let header = "Usage: " ++ progName ++ " [options...] \"file name\""
    case getOpt Permute options argv of
      (o, n, []) ->
        if (Reject `elem` o) || (length n /= 1)
          then error (usageInfo header options)
          else return (o, n)
      (_, _, errs) -> error (concat errs ++ usageInfo header options)

-- scanMany :: String -> Either String [Q3.Lexeme]
-- scanMany input = Q3L.runAlex input go
--   where
--     go = do
--       output <- Q3L.alexMonadScan
--       if Q3.token output == Q3.EofToken
--         then pure [output]
--         else (output :) <$> go

main :: IO ()
main = do
  (_, fileList) <- execOpts
  let filename = head fileList
  flag <- doesFileExist filename
  unless flag (error ("File not found: " ++ filename))
  text <- readFile filename
  case Q3P.parseString text of
    Left errMsg -> error errMsg
    Right ast -> putStr $ Q2.pretty $ fromQasm3 $ Q3G.semanticGraphFrom $ Q3S.syntaxTreeFrom ast
