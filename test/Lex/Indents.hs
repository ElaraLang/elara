{-# LANGUAGE QuasiQuotes #-}

module Lex.Indents where

import Elara.Lexer.Lexer
import Elara.Lexer.Token
import Elara.Lexer.Token (Token (TokenEquals, TokenInt, TokenLeftBrace))
import NeatInterpolation (text)
import Test.Hspec
import Lex.Common
import Common

spec :: Spec
spec = do
    letIndents

letIndents = describe "Lexes indented let declarations" $ do
    it "Should succeed for valid indentations" $ do
        lexUL [text|let x = 1 |] <=> [TokenLet, TokenVariableIdentifier "x", TokenEquals, TokenLeftBrace, TokenInt 1, TokenRightBrace]