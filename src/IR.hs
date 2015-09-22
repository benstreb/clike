module IR where

import Parser (ParseTree)

data AST = Root
         deriving (Eq, Show)

fromParseTree :: ParseTree -> Either String AST
fromParseTree parseTree = Right Root
