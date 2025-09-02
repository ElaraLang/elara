{-# LANGUAGE OverloadedStrings #-}

module Lex.Common where

import Elara.AST.Region (unlocated)
import Elara.Lexer.Pipeline (runLexPipelinePure)
import Elara.Lexer.Reader
import Elara.Lexer.Token
import Polysemy
import Test.Syd

lex' :: Text -> [Lexeme]
lex' contents = do
    case run $ runLexPipelinePure (readTokensWith "<tests>" (toString contents)) of
        Left e -> error $ toText ("lex fail: " ++ show e)
        Right tokens -> tokens

lexUL :: Text -> [Token]
lexUL = fmap (view unlocated) . lex'

(<~>) :: Text -> Text -> Expectation
(<~>) = shouldBe `on` lexUL

-- | Like '<~>', but converts Indent/Dedents to LeftBrace/RightBrace
(<~!~>) :: Text -> Text -> Expectation
(<~!~>) withIndents withoutIndents = withFrozenCallStack $ (shouldBe `on` (fmap convert . lexUL)) withIndents withoutIndents
  where
    convert TokenIndent = TokenLeftBrace
    convert TokenDedent = TokenRightBrace
    convert TokenLineSeparator = TokenSemicolon
    convert t = t
