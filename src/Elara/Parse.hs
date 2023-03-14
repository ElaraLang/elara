module Elara.Parse where

import Elara.AST.Module (Module)
import Elara.AST.Select
import Elara.Lexer.Lexer (Lexeme)
import Elara.Parse.Error
import Elara.Parse.Module (module')
import Elara.Parse.Primitives (toParsec)
import Text.Megaparsec (MonadParsec (eof), runParser)
import Elara.Parse.Stream

parse :: FilePath ->TokenStream  -> Either (WParseErrorBundle TokenStream ElaraParseError) (Module Frontend)
parse y = first WParseErrorBundle . runParser (toParsec module' <* eof) y