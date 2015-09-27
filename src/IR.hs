module IR
    ( AST( Root )
    , Assign( Assign )
    , Value( Int, Func )
    , fromParseTree
    , topLevel
    , retValue
    )
where

import Control.Applicative
import Control.Monad.Except
import qualified Parser ( ParseTree( Assign, Func, Id, Num ) )

data AST = Root
         deriving (Eq, Show)

data Assign = Assign String Value
            deriving (Eq, Show)

data Value = Int Integer
           | Func { retValue :: Value }
           deriving (Eq, Show)

fromParseTree :: Parser.ParseTree -> Either String AST
fromParseTree parseTree = Right Root

topLevel :: [Parser.ParseTree] -> Either String [Assign]
topLevel assigns = forM assigns matchAssign
    where
        matchAssign :: Parser.ParseTree -> Either String IR.Assign
        matchAssign (Parser.Assign (Parser.Id name) valueTree) = case valueTree of
            Parser.Num n -> return (IR.Assign name (Int n))
            Parser.Func _ _ -> return (IR.Assign name (IR.Func $ IR.Int 0))
            _ -> throwError "Not Implemented Yet: Unsupported LHS type"
        matchAssign _ = throwError "ICE: Malformed assignment statement"
