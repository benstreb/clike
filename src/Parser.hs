module Parser
    ( Parser.root
    , Parser.stmt
    , Parser.expr
    ) where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Lexer

data ParseTree = Root [ParseTree]
               | Expr ParseTree
               | Id String
               deriving Show

root :: CharParser () ParseTree
root = Root <$> many stmt

stmt :: CharParser () ParseTree
stmt = Expr <$> expr <* char ';'

expr :: CharParser () ParseTree
expr = Id <$> identifier
