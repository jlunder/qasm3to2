module Main where

import System.Environment (getArgs)

import Qasm3To2
import qualified Qasm2 as Q2
import qualified Qasm3 as Q3
import qualified Qasm3Lexer

-- q3Test = Q3.Program (Q3.VersionSpecifier "3.0") []

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (head args)
  --putStr $ Q2.pretty $ Qasm3To2.toQasm2 q3Test
  print $ Qasm3Lexer.scanner content


-- data Flag
--   = Reject
--   deriving (Show, Eq)

-- options :: [OptDescr Flag]
-- options =
--   []

-- execOpts :: IO ([Flag], [String])
-- execOpts =
--   do
--     argv <- getArgs
--     progName <- getProgName
--     let header = "Usage: " ++ progName ++ " [options...] \"file name\""
--     case (getOpt Permute options argv) of
--       (o, n, []) ->
--         if ((Reject `elem` o) || (length n /= 1))
--           then error (usageInfo header options)
--           else return (o, n)
--       (_, _, errs) -> error (concat errs ++ usageInfo header options)

-- main :: IO ()
-- main =
--   do
--     (_, fileList) <- execOpts
--     let filename = head fileList
--     flag <- doesFileExist filename
--     when (not flag) (error ("The following file does not exist : " ++ filename))
--     putStrLn ("Beginning analysis of the Tiger program in file " ++ head fileList)
--     s <- readFile filename
--     let sr = scanner s
--     case sr of
--       Left st -> error st
--       Right ls -> putStrLn (show ls)
