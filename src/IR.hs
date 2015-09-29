module IR
    ( AST( Root )
    , TopLevel( TopLevel )
    , Value( Int, Func )
    , fromParseTree
    , topLevel
    , body
    )
where

import Control.Applicative
import Control.Monad.Except
import qualified Parser ( ParseTree( Assign, Func, Id, Num ) )

data AST = Root
         deriving (Eq, Show)

data TopLevel = TopLevel String Value
            deriving (Eq, Show)

data Block = Bind Name Value
          deriving (Eq, Show)

data Value = Int Integer
           | Func { body :: [Block] }
           deriving (Eq, Show)

data Name = Name String
          deriving (Eq, Show)

fromParseTree :: Parser.ParseTree -> Either String AST
fromParseTree parseTree = Right Root

topLevel :: [Parser.ParseTree] -> Either String [TopLevel]
topLevel assigns = forM assigns matchAssign
    where
        matchAssign :: Parser.ParseTree -> Either String IR.TopLevel
        matchAssign (Parser.Assign (Parser.Id name) valueTree) = case valueTree of
            Parser.Num n -> return (IR.TopLevel name (Int n))
            Parser.Func _ _ -> return (IR.TopLevel name (IR.Func $ []))
            _ -> throwError "Not Implemented Yet: Unsupported LHS type"
        matchAssign _ = throwError "ICE: Malformed assignment statement"
