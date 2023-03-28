module Lex.Common where

import Control.Lens (view)
import Elara.AST.Region (unlocated)
import Elara.Lexer.Reader
import Elara.Lexer.Token
import Elara.Lexer.Utils
import Polysemy
import Test.Hspec

lex' :: HasCallStack => Text -> [Lexeme]
lex' contents =
    case run $ evalLexMonad "" (toString contents) readTokens of
        Left err -> error (show err)
        Right tokens -> tokens

lexUL :: HasCallStack => Text -> [Token]
lexUL = fmap (view unlocated) . lex'

(<~>) :: HasCallStack => Text -> Text -> Expectation
(<~>) = shouldBe `on` lexUL