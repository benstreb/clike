module IR where

import Control.Applicative
import Control.Monad.Except
import Parser ( ParseTree( Assign, Id, Num ) )

data AST = Root
         deriving (Eq, Show)

data Assign = Assign String Value
            deriving (Eq, Show)

data Value = Int Integer
           deriving (Eq, Show)

fromParseTree :: ParseTree -> Either String AST
fromParseTree parseTree = Right Root

topLevel :: [ParseTree] -> Either String [Assign]
topLevel assigns = forM assigns matchAssign
    where
        matchAssign :: ParseTree -> Either String IR.Assign
        matchAssign (Parser.Assign (Id name) valueTree) = case valueTree of
            Parser.Num n -> return (IR.Assign name (Int n))
            _ -> throwError "Not Implemented Yet: Unsupported LHS type"
        matchAssign _ = throwError "ICE: Malformed assignment statement"
