module Lex.Common where

import Control.Lens (view)
import Elara.AST.Region (unlocated)
import Elara.Lexer.Lexer (Lexeme, lex)
import Elara.Lexer.Token

lex' :: HasCallStack => Text -> [Lexeme]
lex' contents =
    case lex "" (encodeUtf8 contents) of
        Left err -> error (show err)
        Right lexemes -> lexemes

lexUL :: HasCallStack => Text -> [Token]
lexUL = fmap (view unlocated) . lex'