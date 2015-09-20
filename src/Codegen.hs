module Codegen
    ( Codegen.mod
    , generate
    ) where

import Control.Monad.Except
import Data.ByteString
import LLVM.General.AST as AST
import LLVM.General.AST.Constant
import LLVM.General.AST.Global as Global
import LLVM.General.AST.Type as Type
import LLVM.General.Context
import LLVM.General.Module as CModule
import LLVM.General.Target

generate :: AST.Module -> IO (Either String ByteString)
generate m = withContext $ \context ->
    joinExcept $ withModuleFromAST context m targetedModule
    where
        joinExcept = fmap join . runExceptT
        targetedModule :: CModule.Module -> IO (Either String ByteString)
        targetedModule cmodule = joinExcept $ withHostTargetMachine $ \target ->
            runExceptT $ moduleObject target cmodule

ret :: Integer -> Terminator
ret n = Ret (Just (ConstantOperand (Int 64 n))) []

block :: BasicBlock
block = BasicBlock (Name "block") [] (Do $ ret 0)

func :: Definition
func = GlobalDefinition $ functionDefaults
    { Global.returnType = Type.i32
    , Global.name = Name "func"
    , Global.basicBlocks = [block]
    }

mod :: AST.Module
mod = defaultModule
    { moduleName = "Test"
    , moduleDefinitions = [func]
    }
