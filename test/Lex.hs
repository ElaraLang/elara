module Lex where

import Arbitrary.Literals
import Arbitrary.Names (genLowerAlphaText, genOpText, genUpperAlphaText)
import Common
import Elara.AST.Name (ModuleName (..))
import Elara.Lexer.Token
import Lex.Common
import Lex.Indents qualified as Indents
import NeatInterpolation (text)
import Relude.Unsafe (read)
import Test.Hspec
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

spec :: Spec
spec = do
    literals
    symbols
    keywords
    identifiers
    comments
    Indents.spec

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

    it "Lexes char literals" $ do
        lexUL "'a'" <=> [TokenChar 'a']
        lexUL [text| '\n' |] <=> [TokenChar '\n']
        lexUL [text| '\'' |] <=> [TokenChar '\'']
        lexUL [text| '\"' |] <=> [TokenChar '"']
        lexUL [text| '\\' |] <=> [TokenChar '\\']
        lexUL [text| '\t' |] <=> [TokenChar '\t']
        lexUL [text| '\r' |] <=> [TokenChar '\r']
        lexUL [text| '\b' |] <=> [TokenChar '\b']
        lexUL [text| '\f' |] <=> [TokenChar '\f']

        lexUL [text| ' ' |] <=> [TokenChar ' ']
        lexUL [text| 'g' |] <=> [TokenChar 'g']

    it "Lexes string literals" $ do
        lexUL [text| "" |] <=> [TokenString ""]
        lexUL [text| "a" |] <=> [TokenString "a"]
        lexUL [text| "abc" |] <=> [TokenString "abc"]
        lexUL [text| "a\ng" |] <=> [TokenString "a\ng"]
        lexUL [text| "a\tb" |] <=> [TokenString "a\tb"]
        lexUL [text| "a\"b" |] <=> [TokenString "a\"b"]
        lexUL [text| "a\\b" |] <=> [TokenString "a\\b"]
        lexUL [text| "a\rb" |] <=> [TokenString "a\rb"]
        lexUL [text| "a\bb" |] <=> [TokenString "a\bb"]
        lexUL [text| "a\fb" |] <=> [TokenString "a\fb"]
        lexUL [text| "\&\&\&f" |] <=> [TokenString "f"]
        lexUL [text| "a b" |] <=> [TokenString "a b"]
        lexUL [text| "\"\"" |] <=> [TokenString "\"\""]

    it "Lexes arbitrary integers" $ hedgehog $ do
        i <- unIntLiteral <$> forAll genIntLiteral
        lexUL i === [TokenInt (read $ toString i)]

    it "Lexes arbitrary floats" $ hedgehog $ do
        i <- unFloatLiteral <$> forAll genFloatLiteral
        lexUL i === [TokenFloat (read $ toString i)]

    it "Lexes arbitrary chars" $ hedgehog $ do
        i <- unCharLiteral <$> forAll genCharLiteral
        lexUL i === [TokenChar (read $ toString i)]

    it "Lexes arbitrary strings" $ hedgehog $ do
        i <- unStringLiteral <$> forAll genStringLiteral
        lexUL i === [TokenString (read $ toString i)]

symbols :: SpecWith ()
symbols = it "Lexes symbols" $ do
    lexUL ";" <=> [TokenSemicolon]
    lexUL "," <=> [TokenComma]
    lexUL "." <=> [TokenDot]
    lexUL ":" <=> [TokenColon]
    lexUL "::" <=> [TokenDoubleColon]
    lexUL "->" <=> [TokenRightArrow]
    lexUL "<-" <=> [TokenLeftArrow]
    lexUL "=>" <=> [TokenDoubleRightArrow]
    lexUL "=" <=> [TokenEquals]
    lexUL "\\" <=> [TokenBackslash]
    lexUL "@" <=> [TokenAt]
    lexUL "(" <=> [TokenLeftParen]
    lexUL ")" <=> [TokenRightParen]
    lexUL "{" <=> [TokenLeftBrace]
    lexUL "}" <=> [TokenRightBrace]
    lexUL "[" <=> [TokenLeftBracket]
    lexUL "]" <=> [TokenRightBracket]

keywords :: SpecWith ()
keywords = it "Lexes keywords" $ do
    lexUL "def" <=> [TokenDef]
    lexUL "let" <=> [TokenLet]
    lexUL "in" <=> [TokenIn]
    lexUL "if" <=> [TokenIf]
    lexUL "then" <=> [TokenThen]
    lexUL "else" <=> [TokenElse]
    lexUL "class" <=> [TokenClass]
    lexUL "data" <=> [TokenData]
    lexUL "type" <=> [TokenType]
    lexUL "module" <=> [TokenModule]
    lexUL "match" <=> [TokenMatch]
    lexUL "with" <=> [TokenWith]

identifiers :: Spec
identifiers = describe "Lexes identifiers" $ do
    it "Lexes var identifiers" $ do
        lexUL "a" <=> [TokenVariableIdentifier "a"]
        lexUL "abc" <=> [TokenVariableIdentifier "abc"]
        lexUL "a1" <=> [TokenVariableIdentifier "a1"]
        lexUL "a1b2c3" <=> [TokenVariableIdentifier "a1b2c3"]

    it "Lexes con identifiers " $ do
        lexUL "A" <=> [TokenConstructorIdentifier "A"]
        lexUL "ABC" <=> [TokenConstructorIdentifier "ABC"]
        lexUL "A1" <=> [TokenConstructorIdentifier "A1"]
        lexUL "A1B2C3" <=> [TokenConstructorIdentifier "A1B2C3"]
        lexUL "Maybe" <=> [TokenConstructorIdentifier "Maybe"]

    it "Lexes operator identifiers " $ do
        lexUL "+" <=> [TokenOperatorIdentifier "+"]
        lexUL "++" <=> [TokenOperatorIdentifier "++"]
        lexUL "+++" <=> [TokenOperatorIdentifier "+++"]
        lexUL "==" <=> [TokenOperatorIdentifier "=="]
        lexUL "<=>" <=> [TokenOperatorIdentifier "<=>"] -- hehe <=> ception
        lexUL ">>=" <=> [TokenOperatorIdentifier ">>="]
        lexUL ">>>" <=> [TokenOperatorIdentifier ">>>"]
        lexUL ">>" <=> [TokenOperatorIdentifier ">>"]
        lexUL ">>-" <=> [TokenOperatorIdentifier ">>-"]
        lexUL "<$>" <=> [TokenOperatorIdentifier "<$>"]
        lexUL "<$-" <=> [TokenOperatorIdentifier "<$-"]
        lexUL "+--" <=> [TokenOperatorIdentifier "+--"]
        lexUL ".=" <=> [TokenOperatorIdentifier ".="]
        lexUL "A.!." <=> [TokenQOperatorIdentifier (ModuleName (pure "A"), "!.")]

    it "Lexes arbitrary operator identifiers" $ hedgehog $ do
        i <- forAll genOpText
        lexUL i === [TokenOperatorIdentifier i]

    it "Lexes arbitrary variable identifiers" $ hedgehog $ do
        i <- forAll genLowerAlphaText
        lexUL i === [TokenVariableIdentifier i]

    it "Lexes arbitrary constructor identifiers" $ hedgehog $ do
        i <- forAll genUpperAlphaText
        lexUL i === [TokenConstructorIdentifier i]

comments :: Spec
comments = describe "Lexes comments correctly" $ do
    it "Ignores contents after a line comment" $ do
        lexUL "-- the rest is ignored" <=> []
        lexUL "12 -- the rest is ignored" <=> [TokenInt 12]
