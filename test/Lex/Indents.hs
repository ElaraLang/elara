{-# LANGUAGE QuasiQuotes #-}

module Lex.Indents where

import Lex.Common
import NeatInterpolation (text)
import Test.Syd

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
