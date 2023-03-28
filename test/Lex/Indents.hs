{-# LANGUAGE QuasiQuotes #-}

module Lex.Indents where

import Common
import Elara.Lexer.Lexer
import Elara.Lexer.Token
import Elara.Lexer.Token (Token (TokenEquals, TokenInt, TokenLeftBrace))
import Lex.Common
import NeatInterpolation (text)
import Test.Hspec

spec :: Spec
spec = do
    letIndents

letIndents :: Spec
letIndents = describe "Lexes indented let declarations" $ do
    it "Should lex indentation as expected" $ do
        lexUL [text|let x = 1 |] <=> [TokenLet, TokenVariableIdentifier "x", TokenEquals, TokenInt 1]
        [text|
        let x =
                1|]
            <~> "let x = { 1 }"

        [text|
        let x =
                1
                2|]
            <~> "let x = { 1; 2 }"

        [text|
        let x =
                1
                2
                3|]
            <~> "let x = { 1; 2; 3 }"

        [text|
        let x = 
            1
                    2
            3|]
            <~> "let x = { 1 { 2 }; 3}"
        [text|
        let x = 
            1
            2
                3|]
            <~> "let x = { 1 ; 2 { 3 } }"
