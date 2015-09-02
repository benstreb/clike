module Parser
    ( Parser.root
    , Parser.stmts
    , Parser.expr
    , Parser.ParseTree (Assign, Func, Arg, Id)
    ) where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Lexer

data ParseTree = Root [ParseTree]
               | Assign {id :: ParseTree, value :: ParseTree}
               | Func {args :: [ParseTree], body :: [ParseTree] }
               | Arg String
               | Id String
               deriving (Show, Eq)

root :: CharParser () ParseTree
root = Root <$> stmts

stmts :: CharParser () [ParseTree]
stmts = expr `endBy` operator ";"

expr :: CharParser () ParseTree
expr = Id <$> identifier
    <|> Func <$> (reserved "fn" *> parens (arg `sepBy` comma)) <*> braces stmts
    <|> Assign <$> (reserved "let" *> (Id <$> identifier)) <*> (operator "=" *> expr)

arg :: CharParser () ParseTree
arg = Arg <$> identifier
