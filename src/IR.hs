module IR
    ( AST( Root )
    , TopLevel( TopLevel )
    , Value( Int, Func )
    , Block( Block )
    , BlockEnd( Ret )
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

data Block = Block [Stmt] BlockEnd
           deriving (Eq, Show)

data BlockEnd = Ret Value
              deriving (Eq, Show)

data Stmt = Bind Name Value
          deriving (Eq, Show)

data Value = Int Integer
           | Func { body :: Block }
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
            Parser.Func _ body -> IR.TopLevel name <$> IR.Func <$> stmts body
            _ -> throwError "Not Implemented Yet: Unsupported LHS type"
        matchAssign _ = throwError "ICE: Malformed assignment statement"

stmts :: [Parser.ParseTree] -> Either String IR.Block
stmts tree = Right $ Block [] $ Ret $ Int 0
