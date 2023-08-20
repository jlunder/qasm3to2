{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Qasm3To2 where

import Qasm2 qualified
import Qasm3 qualified

toQasm2 :: Qasm3.ProgramNode -> Qasm2.ProgramNode
toQasm2 qasm3 = Qasm2.Program (Qasm2.Real "2.0") []
