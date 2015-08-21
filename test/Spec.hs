import Test.Hspec
import Test.Hspec.QuickCheck
import Lexer
import Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "identifier" $ do
        let parseIdentifier = parse identifier "(test)"
        it "parses a character" $
            parseIdentifier "a" `shouldBe` Right "a"
    describe "integer" $ do
        let parseInteger = parse integer "(test)"
        it "parses a positive integer" $
            parseInteger "1" `shouldBe` Right 1
    describe "reserved" $ do
        let parseReserved word = parse (reserved word) "(test)"
        it "has fn as a reserved word" $
            parseReserved "fn" "fn" `shouldBe` Right ()
