module Main where

import Control.Monad
import Data.Either
import Qasm2 qualified as Q2
import Qasm3 qualified as Q3
import Qasm3Parser qualified as Q3P
import Qasm3To2
import System.Environment (getArgs)

-- q3Test = Q3.Program (Q3.VersionSpecifier "3.0") []

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \a -> do
    content <- readFile a
    -- putStr $ Q2.pretty $ Qasm3To2.toQasm2 q3Test
    putStrLn $ either id show (Q3P.parseQasm3String content)

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
