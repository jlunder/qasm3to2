module Main where

import Control.Monad
import Data.Either
import Qasm2 qualified as Q2
import Qasm3 qualified as Q3
import Qasm3Lexer qualified as Q3L
import Qasm3Parser qualified as Q3P
import Qasm3To2
import System.Environment (getArgs)

scanMany :: String -> Either String [Q3.Lexeme]
scanMany input = Q3L.runAlex input go
  where
    go = do
      output <- Q3L.alexMonadScan
      if Q3.token output == Q3.EofToken
        then pure [output]
        else (output :) <$> go

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \a -> do
    content <- readFile a
    -- putStr $ Q2.pretty $ Qasm3To2.toQasm2 q3Test
    putStrLn $ either ("Lex error: " ++) (\lex -> "Lex:\n" ++ show (map Q3.token lex)) (scanMany content)
    putStrLn ""
    putStrLn $ either ("Parse error: " ++) (\ast -> "Parse:\n" ++ show ast) (Q3P.parseQasm3String content)

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
