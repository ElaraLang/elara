{-# LANGUAGE OverloadedStrings #-}

module Lex.Common where

import Control.Lens (view)
import Elara.AST.Region (unlocated)
import Elara.Lexer.Pipeline (runLexPipeline, runLexPipelinePure)
import Elara.Lexer.Reader
import Elara.Lexer.Token
import Elara.Lexer.Utils
import Elara.Pipeline (finalisePipeline)
import Polysemy
import Polysemy.Error (runError)
import Test.Hspec

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
(<~!~>) = shouldBe `on` (fmap convert . lexUL)
  where
    convert TokenIndent = TokenLeftBrace
    convert TokenDedent = TokenRightBrace
    convert t = t
