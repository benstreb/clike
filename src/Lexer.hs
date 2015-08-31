module Lexer
    ( Lexer.identifier
    , Lexer.integer
    , Lexer.reserved
    , Lexer.operator
    , Lexer.parens
    , Lexer.braces
    , Lexer.comma
    ) where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.Parsec.Language (javaStyle)
import Text.Parsec.Token as T

clikeDef :: T.LanguageDef st
clikeDef = javaStyle
        { reservedNames = ["fn", "let"]
        }

lexer :: T.TokenParser st
lexer = makeTokenParser clikeDef

identifier :: CharParser () String
identifier = T.identifier lexer

integer :: CharParser () Integer
integer = T.integer lexer

reserved :: String -> CharParser () ()
reserved = T.reserved lexer

operator :: String -> CharParser () ()
operator = T.reservedOp lexer

parens :: CharParser () a -> CharParser () a
parens = T.parens lexer

braces :: CharParser () a -> CharParser () a
braces = T.braces lexer

comma :: CharParser () String
comma = T.comma lexer
