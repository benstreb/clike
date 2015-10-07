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

ret :: IR.BlockEnd -> Terminator
ret (IR.Ret n) = Ret (Just (ConstantOperand (constValue n))) []

block :: IR.Block -> BasicBlock
block (IR.Block _ end) = BasicBlock (Name "block") [] (Do $ ret end)

global :: IR.TopLevel -> Definition
global (IR.TopLevel name value) = GlobalDefinition $ case value of
    IR.Func blocks -> functionDefaults
        { Global.returnType = Type.i32
        , Global.name = Name name
        , Global.basicBlocks = [block $ IR.Block [] $ IR.Ret $ IR.Int 0]
        }
    IR.Int _ -> globalVariableDefaults
        { Global.name = Name name
        , Global.initializer = Just $ constValue value
        , Global.type' = Type.IntegerType 64
        }

func :: Definition
func = global $ IR.TopLevel "func" $ IR.Func $ IR.Block [] $ IR.Ret $ IR.Int 0

mod :: AST.Module
mod = defaultModule
    { moduleName = "Test"
    , moduleDefinitions = [func]
    }
