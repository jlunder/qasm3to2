{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fmap" #-}
module Qasm3To2Tests where

import Ast qualified
import Chatty
import Control.Monad
import Data.Either (fromRight)
import Data.Functor
import Debug.Trace (trace)
import Qasm2 qualified
import Qasm3.Parser qualified as Q3P
import Qasm3.Syntax
import Qasm3.Test.Arbitrary qualified as Q3A
import System.Exit (exitFailure)
import System.IO
import Test.HUnit
import Test.QuickCheck

testAstEquivalence = TestLabel "AST Equivalence" $ TestCase $ do
  -- ast = AstNode ...

  genAst <- generate (Q3A.arbitraryProgramNode cfg)

  putStrLn $ "Original AST:\n" ++ show genAst ++ "\n"
  hFlush stdout
  let str = pretty genAst
  putStrLn $ "Generated source:\n" ++ str ++ "\n"
  hFlush stdout
  let parseResult = Q3P.parseString str <&> syntaxTreeFrom
  let ast = syntaxTreeFrom genAst
  putStrLn $ "Parse result:\n" ++ show parseResult ++ "\n"
  hFlush stdout

  assertBool "Round-Trip AST Equivalent" (fromChattyValue Ast.NilNode parseResult == ast)

tests =
  TestList
    [ testAstEquivalence
    ]

cfg = Q3A.defaultConfig

prop_roundTrip = forAll (Q3A.arbitraryProgramNode cfg) $
  \ast -> syntaxTreeFrom ast == syntaxTreeFrom (fromChattyValue Ast.NilNode $ Q3P.parseString (pretty ast))

main :: IO ()
main = do
  count <- runTestTT tests
  unless (failures count == 0) exitFailure

  result <- verboseCheckResult (withMaxSuccess 10000 prop_roundTrip)
  unless (isSuccess result) exitFailure
