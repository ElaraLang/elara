module Lex.Common where

import Control.Lens (view)
import Elara.AST.Region (unlocated)
import Elara.Lexer.Reader
import Elara.Lexer.Token
import Elara.Lexer.Utils
import Test.Hspec

lex' :: Text -> [Lexeme]
lex' contents =
    case evalLexMonad "" (toString contents) readTokens of
        Left _ -> error "lex fail aaa"
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
 