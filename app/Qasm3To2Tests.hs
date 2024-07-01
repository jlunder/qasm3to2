{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fmap" #-}
module Qasm3To2Tests where

import Ast
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
  genAst <- generate (Q3A.arbitraryProgramNode cfg)

  let str = pretty genAst
  let parseResult = Q3P.parseString str <&> syntaxTreeFrom
  let ast = syntaxTreeFrom genAst
  let isEquivalent = fromChattyValue Ast.NilNode parseResult == ast
  unless
    isEquivalent
    ( do
        hFlush stdout
        putStrLn ""
        putStrLn $ "Emitted source:\n" ++ str ++ "\n"
        putStrLn $ "Original AST:\n" ++ show genAst ++ "\n"
        putStrLn $ "Parsed AST:\n" ++ show parseResult ++ "\n"
        hFlush stdout
    )

  assertBool "Round-Trip AST Equivalent" (fromChattyValue Ast.NilNode parseResult == ast)

makeFileParseTest baseName =
  TestLabel ("Parse " ++ baseName ++ ".qasm") $
    TestCase $
      testParseExampleFiles (baseName ++ ".qasm") (baseName ++ ".ast")

makeCustomFileParseTest qasmName astName =
  TestLabel ("Parse " ++ qasmName) $ TestCase $ testParseExampleFiles qasmName astName

makeParseTest name !qasmStr !expectedAst =
  TestLabel ("Parse " ++ name) $ TestCase $ testParseExample qasmStr expectedAst

testParseExampleFiles qasmName astName = do
  !qasmStr <- readFile qasmName
  !astStr <- readFile astName
  let !expectedAst = read astStr
  testParseExample qasmStr expectedAst

testParseExample !qasmStr !expectedAst = do
  let !parseResult = Q3P.parseString qasmStr <&> syntaxTreeFrom
  let resultMessage = case parseResult of
        Chatty.ChattyFailure msgs _ -> concat msgs
        Chatty.ChattyValue _ ast -> "Pretty-printed parse AST:\n" ++ pretty ast
  assertBool resultMessage (hasChattyValue parseResult)
  let parsedAst = fromChattyValue Ast.NilNode parseResult
  let isEquivalent = parsedAst == expectedAst
  assertEqual resultMessage expectedAst parsedAst

basicTest =
  let qasmStr =
        "qubit[4] q;\n"
          ++ "bit[4] c;\n"
          ++ "c[0] = measure q[0];"
      expectedAst = syntaxTreeFrom $ programNode Nothing Nothing body
        where
          body =
            [ stmtNode QuantumDeclStmt [node QubitTypeSpec [integerLiteralNode 4], identifierNode "q"],
              stmtNode ClassicalDeclStmt [node BitTypeSpec [integerLiteralNode 4], identifierNode "c", NilNode],
              stmtNode
                (AssignmentStmt EqualsToken)
                [ node IndexExpr [identifierNode "c", node List [integerLiteralNode 0]],
                  node MeasureExpr [node IndexedIdentifier [identifierNode "q", node List [integerLiteralNode 0]]]
                ]
            ]
   in makeParseTest "basic test" qasmStr expectedAst

tests =
  TestList $
    [basicTest]
      ++ map
        makeFileParseTest
        [ "test-data/00-trivial",
          "test-data/01-end",
          "test-data/10-basic",
          "openqasm-examples/adder",
          "openqasm-examples/alignment",
          "openqasm-examples/arrays",
          "openqasm-examples/cphase",
          "openqasm-examples/dd",
          "openqasm-examples/defcal",
          "openqasm-examples/gateteleport",
          "openqasm-examples/inverseqft1",
          "openqasm-examples/inverseqft2",
          "openqasm-examples/ipe",
          "openqasm-examples/msd",
          "openqasm-examples/qec",
          "openqasm-examples/qft",
          "openqasm-examples/qpt",
          "openqasm-examples/rb",
          "openqasm-examples/rus",
          "openqasm-examples/scqec"
        ]
      ++ [makeCustomFileParseTest "openqasm-examples/stdgates.inc" "openqasm-examples/stdgates.ast"]
      ++ map
        makeFileParseTest
        [ "openqasm-examples/t1",
          "openqasm-examples/teleport",
          "openqasm-examples/varteleport",
          "openqasm-examples/vqe"
        ]

cfg = Q3A.defaultConfig

prop_roundTrip = forAll (Q3A.arbitraryProgramNode cfg) $
  \ast -> syntaxTreeFrom ast == syntaxTreeFrom (fromChattyValue Ast.NilNode $ Q3P.parseString (pretty ast))

node tag children = Node {tag = tag, children = children, context = ()}

programNode :: Maybe Int -> Maybe Int -> [Ast.Node Tag ()] -> Ast.Node Tag ()
programNode (Just maj) (Just min) =
  node (Program maj (Just min) (VersionSpecifierToken (show maj ++ "." ++ show min)))
programNode (Just maj) Nothing = node (Program maj Nothing (VersionSpecifierToken $ show maj))
programNode Nothing Nothing = node (Program 3 Nothing EofToken)

stmtNode :: Tag -> [Ast.Node Tag ()] -> Ast.Node Tag ()
stmtNode tag children = node Statement [node tag children]

withAnnotations :: Ast.Node Tag () -> [Ast.Node Tag ()] -> Ast.Node Tag ()
withAnnotations (Ast.Node {tag = Statement, children = [stmtNode]}) annotations =
  node Statement $ stmtNode : annotations

identifierNode :: String -> Ast.Node Tag ()
identifierNode name = node (Identifier {identifierName = name, identifierTok = IdentifierToken name}) []

integerLiteralNode :: Integer -> Ast.Node Tag ()
integerLiteralNode val =
  node (IntegerLiteral {integerVal = val, integerTok = DecimalIntegerLiteralToken (show val)}) []

floatLiteralNode :: Double -> Ast.Node Tag ()
floatLiteralNode val =
  node (FloatLiteral {floatVal = val, floatTok = FloatLiteralToken (show val)}) []

imaginaryLiteralNode :: Double -> Ast.Node Tag ()
imaginaryLiteralNode val =
  node (ImaginaryLiteral {imaginaryVal = val, imaginaryTok = ImaginaryLiteralToken (show val ++ "im")}) []

booleanLiteralNode :: Bool -> Ast.Node Tag ()
booleanLiteralNode val =
  node (BooleanLiteral {booleanVal = val, booleanTok = BooleanLiteralToken (if val then "true" else "false")}) []

bitstringLiteralNode :: [Bool] -> Ast.Node Tag ()
bitstringLiteralNode val =
  node
    ( BitstringLiteral
        { bitstringVal = val,
          bitstringTok = BitstringLiteralToken $ '"' : map (\b -> if b then '1' else '0') val ++ "\""
        }
    )
    []

main :: IO ()
main = do
  count <- runTestTT tests
  unless ((failures count == 0) && (errors count == 0)) exitFailure

-- result <- verboseCheckResult (withMaxSuccess 10000 prop_roundTrip)
-- unless (isSuccess result) exitFailure

--  0: basic test                     -- OK
--  1: test-data/00-trivial           -- OK
--  2: test-data/01-end               -- OK
--  3: test-data/10-basic             -- OK
--  4: openqasm-examples/adder        -- OK
--  5: openqasm-examples/alignment    -- OK
--  6: openqasm-examples/arrays       -- Prelude.read: no parse
--  7: openqasm-examples/cphase       -- OK
--  8: openqasm-examples/dd           -- OK
--  9: openqasm-examples/defcal       -- OK
-- 10: openqasm-examples/gateteleport -- OK
-- 11: openqasm-examples/inverseqft1  -- OK
-- 12: openqasm-examples/inverseqft2  -- OK
-- 13: openqasm-examples/ipe          -- OK
-- 14: openqasm-examples/msd          -- Error: Parse error: 53, 11: unexpected IdentifierToken "scratch", stopped at 53, 18
-- 15: openqasm-examples/qec          -- OK
-- 16: openqasm-examples/qft          -- OK
-- 17: openqasm-examples/qpt          -- OK
-- 18: openqasm-examples/rb           -- OK
-- 19: openqasm-examples/rus          -- OK
-- 20: openqasm-examples/scqec        -- OK
-- 21: openqasm-examples/stdgates     -- Error: Parse error: 22, 25: unexpected IdentifierToken "a", stopped at 22, 26
-- 22: openqasm-examples/t1           -- OK
-- 23: openqasm-examples/teleport     -- OK
-- 24: openqasm-examples/varteleport  -- OK
-- 25: openqasm-examples/vqe          -- OK

--ast = 
