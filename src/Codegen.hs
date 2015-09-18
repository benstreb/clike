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
generate m = withContext ctxt
    where
        ctxt :: Context -> IO (Either String ByteString)
        ctxt context = fmap join $ runExceptT $ withModuleFromAST context m targetedModule
            where
                targetedModule :: CModule.Module -> IO (Either String ByteString)
                targetedModule cmodule = fmap join $ runExceptT $ withHostTargetMachine makeModule
                    where
                        makeModule :: TargetMachine -> IO (Either String ByteString)
                        makeModule target = runExceptT $ moduleObject target cmodule

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

mod :: AST.Module
mod = defaultModule
    { moduleName = "Test"
    , moduleDefinitions = [func]
    }
