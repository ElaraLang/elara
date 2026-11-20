{-# LANGUAGE QuasiQuotes #-}

module Lex.Indents where

import NeatInterpolation (text)
import Test.Syd

import Elara.AST.Region (endPos, sourceRegion, startPos, unlocated, _RealSourceRegion)
import Elara.Lexer.Token
import Lex.Common

spec :: Spec
spec = do
    letIndents

letIndents :: Spec
letIndents = describe "Lexes indented let declarations" $ do
    it "Should lex indentation as expected" $ do
        [text|
        let x =
                1|]
            <~!~> "let x = { 1 }"

        [text|
        let x =
                1
                2|]
            <~!~> "let x = { 1; 2 }"

        [text|
        let x =
                1
                2
                3|]
            <~!~> "let x = { 1; 2; 3 }"

        -- When 2 is more indented than 1, it's a continuation (no new block)
        -- When 3 is at the same level as 1, it's a new statement (semicolon)
        [text|
        let x =
            1
                    2
            3|]
            <~!~> "let x = { 1 2; 3}"

        -- Same logic: 3 is a continuation of 2, not a new block
        [text|
        let x =
            1
            2
                3|]
            <~!~> "let x = { 1 ; 2 3 }"

    it "Lexes nested lets properly" $ do
        [text|
        let x =
                let y = 
                        1
                in y
        in x|]
            <~!~> "let x = { let y = { 1 }; in y }; in x"

    it "Doesn't emit layout tokens at the end of the input" $ do
        let block1 = [TokenLet, TokenVariableIdentifier "x", TokenEquals, TokenIndent, TokenInt 1]
        lexUL "let x =\n        1\n" `shouldBe` block1
        lexUL "let x =\n        1\n\n\n" `shouldBe` block1
        lexUL "let x =\n        1\n        -- trailing comment\n" `shouldBe` block1
        lexUL "let x =\n        let y =\n                1\n"
            `shouldBe` [TokenLet, TokenVariableIdentifier "x", TokenEquals, TokenIndent, TokenLet, TokenVariableIdentifier "y", TokenEquals, TokenIndent, TokenInt 1]

    it "Places layout breaks at the end of the last real token" $ do
        let lexemes = lex' "let x =\n    1\n\n    -- a comment\nlet y = 2"
            layoutBreaks = filter (isLayoutBreak . view unlocated) lexemes
            startOf l = l ^? sourceRegion % _RealSourceRegion % startPos
            endOf l = l ^? sourceRegion % _RealSourceRegion % endPos
        case find ((== TokenInt 1) . view unlocated) lexemes of
            Nothing -> expectationFailure "expected a token for 1"
            Just one -> do
                layoutBreaks `shouldSatisfy` (not . null)
                for_ layoutBreaks $ \l -> (startOf l, endOf l) `shouldBe` (endOf one, endOf one)

    it "Places a dedent before a closing delimiter at the end of the last token before it" $ do
        let lexemes = lex' "f (\\x ->\n    x)"
            endOf l = l ^? sourceRegion % _RealSourceRegion % endPos
            lastX = viaNonEmpty last (filter ((== TokenVariableIdentifier "x") . view unlocated) lexemes)
            dedent = find ((== TokenDedent) . view unlocated) lexemes
        case (lastX, dedent) of
            (Just x, Just d) -> endOf d `shouldBe` endOf x
            _ -> expectationFailure "expected an x token and a dedent"
