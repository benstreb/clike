module Parser
    ( Parser.root
    , Parser.stmt
    , Parser.expr
    , Parser.ParseTree (Func, Id)
    ) where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Lexer

data ParseTree = Root [ParseTree]
               | Expr ParseTree
               | Func {args :: [ParseTree], body :: [ParseTree] }
               | Id String
               deriving (Show, Eq)

root :: CharParser () ParseTree
root = Root <$> many stmt

stmt :: CharParser () ParseTree
stmt = Expr <$> expr <* char ';'

expr :: CharParser () ParseTree
expr = Id <$> identifier
    <|> Func <$> (reserved "fn" *> parens (arg `sepBy` comma)) <*> braces (many stmt)

arg :: CharParser () ParseTree
arg = Id <$> identifier
