{-# LANGUAGE OverloadedStrings #-}

module Lex.Common where

import Effectful (runPureEff)
import Effectful.Error.Static (runError)
import Elara.AST.Region (unlocated)
import Elara.Lexer.Reader
import Elara.Lexer.Token
import Elara.Logging (ignoreStructuredDebug)
import Elara.ReadFile (FileContents (..))
import Test.Syd

lex' :: Text -> [Lexeme]
lex' contents = do
    case runPureEff $
        runError $
            ignoreStructuredDebug (readTokensWith (FileContents "<tests>" contents)) of
        Left (e, _) -> error $ toText ("lex fail: " ++ show e)
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
