{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Qasm3To2 where

import qualified Qasm2
import qualified Qasm3

toQasm2 :: Qasm3.ProgramNode -> Qasm2.ProgramNode
toQasm2 qasm3 = Qasm2.Program (Qasm2.Real "2.0") []

