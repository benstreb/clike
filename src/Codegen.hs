module Codegen
    ( block
    ) where

import LLVM.General.AST
import LLVM.General.AST.Constant
import LLVM.General.AST.Global as Global
import LLVM.General.AST.Type as Type

return0 :: Terminator
return0 = Ret (Just (ConstantOperand (Int 0 0))) []

block :: BasicBlock
block = BasicBlock (Name "block") [] (Do return0)

func :: Definition
func = GlobalDefinition $ functionDefaults
    { Global.returnType = Type.i32
    , Global.name = Name "func"
    , Global.basicBlocks = [block]
    }

mod :: Module
mod = defaultModule
    { moduleName = "Test"
    , moduleDefinitions = [func]
    }
