import Test.Hspec
import Test.Hspec.QuickCheck
import Lexer
import Parser
import qualified IR
import Codegen
import Text.ParserCombinators.Parsec (parse, ParseError, string)

main :: IO ()
main = do
    hspec lexerSpec
    hspec parserSpec
    hspec irSpec
    hspec codegenSpec

isError :: Either a b -> Bool
isError e = case e of
    Right _ -> False
    Left _ -> True

codegenSpec :: Spec
codegenSpec = do
    describe "testing llvm-general" $ do
        it "generates a trivial module" $ do
            generatedModule <- generate Codegen.mod
            generatedModule `shouldNotSatisfy` isError

irSpec :: Spec
irSpec = do
    describe "root" $ do
        it "doesn't fail, given an empty AST" $ do
            IR.fromParseTree (Root []) `shouldNotSatisfy` isError
    describe "topLevel" $ do
        it "handles assignments of integer values" $
            IR.topLevel [Assign (Id "value") (Num 3)] `shouldBe` Right [IR.TopLevel "value" (IR.Int 3)]
        it "handles assignments of trivial functions" $
            IR.topLevel [Assign (Id "f") (Func {args=[], body=[]})]
                `shouldBe` Right [IR.TopLevel "f" (IR.Func {IR.body=IR.Block [] $ IR.Ret $ IR.Int 0})]
    describe "value" $ do
        it "handles a number" $
            IR.value (Num 3) `shouldBe` Right (IR.Int 3)
        it "handles a trivial function" $
            IR.value (Func {args=[], body=[]})
                `shouldBe` Right (IR.Func {IR.body=IR.Block [] $ IR.Ret $ IR.Int 0})

parserSpec :: Spec
parserSpec = do
    describe "root" $ do
        it "parses a sample program" $
            parse root "(test)" "let value = 1; let main = fn() {};" `shouldNotSatisfy` isError
        it "fails even when the program is invalid on the first token" $
            parse root "(test)" "}" `shouldSatisfy` isError
    describe "assign" $ do
        let assignParse = parse assign "(test)"
        it "can be an assignment statement" $
            assignParse "let a = b" `shouldBe` Right (Assign (Id "a") (Id "b"))
        it "supports assigning a function" $
            assignParse "let f = fn() {}" `shouldBe` Right (Assign (Id "f") (Func [] []))
    describe "return" $ do
        let returnParse = parse return_ "(test)"
        it "can return an integer" $
            returnParse "return 3" `shouldBe` Right (Return (Num 3))
    describe "expr" $ do
        let exprParse = parse expr "(test)"
        it "can have a bare identifier" $
            exprParse "abc" `shouldBe` Right (Id "abc")
        it "can have an empty anonymous function" $
            exprParse "fn () {}" `shouldBe` Right (Func [] [])
        it "can have an anonymous function with an argument" $
            exprParse "fn (arg) {expr;}" `shouldBe` Right (Func [Arg "arg"] [Id "expr"])
        it "parses a function call" $
            exprParse "f()" `shouldBe` Right (Call (Id "f") [])
        it "parses successive function calls" $
            exprParse "f()()" `shouldBe` Right (Call (Call (Id "f") []) [])
        it "parses a function call with an argument" $
            exprParse "f(arg)" `shouldBe` Right (Call (Id "f") [Id "arg"])
        it "parses an if statement" $
            exprParse "if test {}" `shouldBe` Right (If (Id "test") [])
        it "parses an integer as a number" $
            exprParse "3" `shouldBe` Right (Num 3)

lexerSpec :: Spec
lexerSpec = do
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
        it "it doesn't treat a word starting with fn as a reserved word" $
            parseReserved "fn" "fn123" `shouldSatisfy` isError
        it "does treat fn as a reserved word if a symbol follows it" $
            parseReserved "fn" "fn()" `shouldBe` Right ()
        it "has let as a reserved word" $
            parseReserved "let" "let" `shouldBe` Right ()
    describe "parens" $ do
        let parseParens inner = parse (parens inner) "(test)"
        it "parses parenthesis and returns what's inside them" $
            parseParens (string "") "()" `shouldBe` Right ""
    describe "braces" $ do
        let parseParens inner = parse (braces inner) "(test)"
        it "parses braces and returns what's inside them" $
            parseParens (string "") "{}" `shouldBe` Right ""
    describe "comma" $ do
        let parseComma = parse comma "(test)"
        it "parses a comma" $
            parseComma "," `shouldBe` Right ","
