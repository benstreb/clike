module Codegen
    ( block
    ) where

import LLVM.General.AST
import LLVM.General.AST.Instruction
import LLVM.General.AST.Constant

block :: BasicBlock
block = BasicBlock (Name "block") [] (Do (Ret (Just (ConstantOperand (Int 0 0))) []))
