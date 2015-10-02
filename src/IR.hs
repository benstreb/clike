module IR
    ( AST( Root )
    , TopLevel( TopLevel )
    , Value( Int, Func )
    , Block( Block )
    , BlockEnd( Ret )
    , fromParseTree
    , topLevel
    , value
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
        matchAssign (Parser.Assign (Parser.Id name) valueTree) = IR.TopLevel name <$> value valueTree
        matchAssign (Parser.Assign _ _) = throwError "ICE: expected an identifier on the left hand side"
        matchAssign _ = throwError "ICE: expected only assignment statements at the top level"

value :: Parser.ParseTree -> Either String Value
value (Parser.Num n) = return $ Int n
value (Parser.Func _ body) = IR.Func <$> stmts body
value _ = throwError "Not Implemented Yet: Unsupported LHS type"

stmts :: [Parser.ParseTree] -> Either String IR.Block
stmts tree = Right $ Block [] $ Ret $ Int 0
