{-# LANGUAGE OverloadedStrings #-}

module Lex.Common where

import Effectful (runPureEff)
import Effectful.Error.Static (runError)
import Test.Syd

import Elara.AST.Region (unlocated)
import Elara.Lexer.Reader
import Elara.Lexer.Token
import Elara.Logging (ignoreStructuredDebug)
import Elara.ReadFile (FileContents (..))

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

{- | Like '<~>', but converts @<INDENT>@s/@<DEDENT>@s to LeftBrace/RightBrace.

Since this code only tests the lexer, we lack the parser-specific
layout handling so have to manually reinsert closing braces after an EOF.

The lexer doesn't close indented blocks that are still open at the end of the input, so @let x =\\n    1@
lexes to @let x = { 1@. Any such blocks on the indented side are closed here to produce @let x = { 1 }@

See <https://elara.readthedocs.io/en/latest/compiler/internals/stages/lexing.html#end-of-input the lexing documentation> for why the lexer leaves them open.
-}
(<~!~>) :: Text -> Text -> Expectation
(<~!~>) withIndents withoutIndents =
    withFrozenCallStack $
        closeOpenBraces (convertLayout (lexUL withIndents)) `shouldBe` convertLayout (lexUL withoutIndents)
  where
    convertLayout = fmap convert
    convert TokenIndent = TokenLeftBrace
    convert TokenDedent = TokenRightBrace
    convert TokenLineSeparator = TokenSemicolon
    convert t = t

    closeOpenBraces tokens = tokens <> replicate (foldl' depth 0 tokens) TokenRightBrace
    depth n TokenLeftBrace = n + 1
    depth n TokenRightBrace = max 0 (n - 1)
    depth n _ = n
