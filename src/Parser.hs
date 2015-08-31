module Parser
    ( Parser.root
    , Parser.stmt
    , Parser.expr
    , Parser.ParseTree (Expr, Func, Id, Arg, Assign)
    ) where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Lexer

data ParseTree = Root [ParseTree]
               | Expr ParseTree
               | Assign {id :: ParseTree, value :: ParseTree}
               | Func {args :: [ParseTree], body :: [ParseTree] }
               | Arg String
               | Id String
               deriving (Show, Eq)

root :: CharParser () ParseTree
root = Root <$> many stmt

stmt :: CharParser () ParseTree
stmt = Expr <$> expr <* char ';'

expr :: CharParser () ParseTree
expr = Id <$> identifier
    <|> Func <$> (reserved "fn" *> parens (arg `sepBy` comma)) <*> braces (many stmt)
    <|> Assign <$> (reserved "let" *> (Id <$> identifier)) <*> (operator "=" *> expr)

arg :: CharParser () ParseTree
arg = Arg <$> identifier
