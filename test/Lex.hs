module Lex where

import Control.Lens (view)
import Elara.AST.Region (unlocated)
import Elara.Lexer.Lexer
import Elara.Lexer.Token
import Test.Hspec

spec :: Spec
spec = do
    literals

literals :: Spec
literals = describe "Lexes literals" $ do
    describe "Lexes integer literals" $ do
        it "Lexes decimal integer literals" $ do
            lexUL "1" <=> [TokenInt 1]
            lexUL "123" <=> [TokenInt 123]
            lexUL "-1" <=> [TokenInt (-1)]
            lexUL "-123" <=> [TokenInt (-123)]
            lexUL "0" <=> [TokenInt 0]
            lexUL "-0" <=> [TokenInt (-0)]

        it "Lexes hexadecimal integer literals" $ do
            lexUL "0x1" <=> [TokenInt 1]
            lexUL "0x123" <=> [TokenInt 0x123]
            lexUL "-0x1" <=> [TokenInt (-0x1)]
            lexUL "-0x123" <=> [TokenInt (-0x123)]
            lexUL "0x0" <=> [TokenInt 0]

        it "Lexes octal integer literals" $ do
            lexUL "0o1" <=> [TokenInt 1]
            lexUL "0o123" <=> [TokenInt 0o123]
            lexUL "-0o1" <=> [TokenInt (-0o1)]
            lexUL "-0o123" <=> [TokenInt (-0o123)]
            lexUL "0o0" <=> [TokenInt 0]

    describe "Lexes float literals" $ do
        it "Lexes decimal float literals" $ do
            lexUL "1.0" <=> [TokenFloat 1.0]
            lexUL "123.456" <=> [TokenFloat 123.456]
            lexUL "-1.0" <=> [TokenFloat (-1.0)]
            lexUL "-123.456" <=> [TokenFloat (-123.456)]
            lexUL "0.0" <=> [TokenFloat 0.0]
            lexUL "-0.0" <=> [TokenFloat (-0.0)]

        it "Lexes exponential float literals" $ do
            lexUL "1e0" <=> [TokenFloat 1e0]
            lexUL "1e1" <=> [TokenFloat 1e1]
            lexUL "1e2" <=> [TokenFloat 1e2]
            lexUL "1e+3" <=> [TokenFloat 1e+3]
            lexUL "1e-1" <=> [TokenFloat 1e-1]
            lexUL "1e-2" <=> [TokenFloat 1e-2]

        it "Lexes decimal and exponential float literals" $ do
            lexUL "123.456e0" <=> [TokenFloat 123.456e0]
            lexUL "123.456e1" <=> [TokenFloat 123.456e1]
            lexUL "123.456e2" <=> [TokenFloat 123.456e2]
            lexUL "123.456e+3" <=> [TokenFloat 123.456e+3]
            lexUL "123.456e-1" <=> [TokenFloat 123.456e-1]
            lexUL "123.456e-2" <=> [TokenFloat 123.456e-2]

(<=>) :: (HasCallStack, Eq a, Show a) => a -> a -> Expectation
(<=>) = shouldBe

lex' :: HasCallStack => Text -> [Lexeme]
lex' contents =
    case lex "" (encodeUtf8 contents) of
        Left err -> error (show err)
        Right lexemes -> lexemes

lexUL :: HasCallStack => Text -> [Token]
lexUL = fmap (view unlocated) . lex'