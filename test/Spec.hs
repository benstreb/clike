import Test.Hspec
import Test.Hspec.QuickCheck
import Lexer
import Text.ParserCombinators.Parsec (parse, ParseError)

main :: IO ()
main = hspec spec

isError :: Either ParseError a -> Bool
isError e = case e of
    Right _ -> False
    Left _ -> True

spec :: Spec
spec = do
    describe "identifier" $ do
        let parseIdentifier = parse identifier "(test)"
        it "parses a character" $
            parseIdentifier "a" `shouldBe` Right "a"
        it "parses a word" $
            parseIdentifier "abc" `shouldBe` Right "abc"
        it "doesn't parse a symbol" $
            parseIdentifier "+" `shouldSatisfy` isError
        it "stops parsing when it sees a symbol" $
            parseIdentifier "a+" `shouldBe` Right "a"
        it "can have digits in an identifier" $
            parseIdentifier "a1" `shouldBe` Right "a1"
        it "can't start with digits" $
            parseIdentifier "1a" `shouldSatisfy` isError
    describe "integer" $ do
        let parseInteger = parse integer "(test)"
        it "parses a positive integer" $
            parseInteger "1" `shouldBe` Right 1
        it "parses a multi-digit integer" $
            parseInteger "100" `shouldBe` Right 100
        it "parses a negative integer" $
            parseInteger "-1" `shouldBe` Right (-1)
        it "stops parsing when it no longer reads an integer" $
            parseInteger "1+" `shouldBe` Right 1
    describe "reserved" $ do
        let parseReserved word = parse (reserved word) "(test)"
        it "has fn as a reserved word" $
            parseReserved "fn" "fn" `shouldBe` Right ()
