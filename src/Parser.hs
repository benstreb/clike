module Parser
    ( Parser.root
    , Parser.stmts
    , Parser.assign
    , Parser.expr
    , Parser.ParseTree (Root, Assign, Func, Arg, Call, If, Id, Num)
    ) where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Lexer

data ParseTree = Root [ParseTree]
               | Assign {id :: ParseTree, value :: ParseTree}
               | Func {args :: [ParseTree], body :: [ParseTree] }
               | Arg String
               | Call {func :: ParseTree, args :: [ParseTree]}
               | If {comp :: ParseTree, body :: [ParseTree]}
               | Id String
               | Num Integer
               deriving (Show, Eq)

root :: CharParser () ParseTree
root = Root <$> stmts

stmts :: CharParser () [ParseTree]
stmts = expr `endBy` operator ";"

assign :: CharParser () ParseTree
assign = Assign <$> (reserved "let" *> (Id <$> identifier)) <*> (operator "=" *> expr)

expr :: CharParser () ParseTree
expr = Func <$> (reserved "fn" *> parens (arg `sepBy` comma)) <*> braces stmts
    <|> If <$> (reserved "if" *> expr) <*> braces stmts
    <|> num
    <|> call

call :: CharParser () ParseTree
call = optCall <$> ident <*> many (parens (expr `sepBy` comma))
    where optCall ident [] = ident
          optCall ident (args:rest) = Call (optCall ident rest) args

ident :: CharParser () ParseTree
ident = Id <$> identifier

num :: CharParser () ParseTree
num = Num <$> integer

arg :: CharParser () ParseTree
arg = Arg <$> identifier
