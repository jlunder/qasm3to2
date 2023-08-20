{
module Qasm3Parser (parseQasm3, parseQasm3String, parseQasm3Lexemes) where

import Data.Char
import Qasm3
import Qasm3Lexer qualified as L
}

%name parseQasm3 empty

%tokentype { L.Lexeme }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.L _ EofToken }

%%

empty : { Program (VersionSpecifier "3") [] }  -- only to get the file compiling; we will remove this

{
parseError :: L.Lexeme -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.Lexeme -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

parseQasm3String :: String -> Maybe ProgramNode
parseQasm3String programStr =
  case L.runAlex programStr parseQasm3 of
    Left err -> Nothing
    Right program -> Just program

parseQasm3Lexemes :: [L.Lexeme] -> Maybe ProgramNode
parseQasm3Lexemes lexemes =
  Just (Program (VersionSpecifier "3") [])
}
