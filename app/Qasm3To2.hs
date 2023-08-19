{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Qasm3To2 where

import qualified Qasm3
import qualified Qasm2

toQasm2 :: Qasm3.ProgramNode -> Qasm2.MainProgramNode
toQasm2 qasm3 = Qasm2.MainProgram (Qasm2.Real "2.0") (Qasm2.Program [])

