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
import qualified IR

generate :: AST.Module -> IO (Either String ByteString)
generate m = withContext $ \context ->
    joinExcept $ withModuleFromAST context m targetedModule
    where
        joinExcept = fmap join . runExceptT
        targetedModule :: CModule.Module -> IO (Either String ByteString)
        targetedModule cmodule = joinExcept $ withHostTargetMachine $ \target ->
            runExceptT $ moduleObject target cmodule

constValue :: IR.Value -> Constant
constValue (IR.Int n) = Int 64 n

ret :: IR.Value -> Terminator
ret n = Ret (Just (ConstantOperand (constValue n))) []

block :: IR.Value -> BasicBlock
block retValue = BasicBlock (Name "block") [] (Do $ ret retValue)

func :: Definition
func = GlobalDefinition $ functionDefaults
    { Global.returnType = Type.i32
    , Global.name = Name "func"
    , Global.basicBlocks = [block $ IR.Int 0]
    }

mod :: AST.Module
mod = defaultModule
    { moduleName = "Test"
    , moduleDefinitions = [func]
    }
