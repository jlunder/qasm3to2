module Qasm2Parser where

import Qasm2

-- parseString :: String -> Either String ProgramNode
-- parseString programStr = L.runAlex programStr parseQasm3
parseString :: String -> Either String ProgramNode
parseString programText = Left "Not implemented!" -- Right (Program (Real "2") [])
