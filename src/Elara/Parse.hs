module Elara.Parse where

import Elara.AST.Module (Module)
import Elara.AST.Select
import Elara.Parse.Error
import Elara.Parse.Module (module')
import Elara.Parse.Primitives (toParsec)
import Text.Megaparsec (MonadParsec (eof), runParser)

parse :: FilePath -> Text -> Either (WParseErrorBundle Text ElaraParseError) (Module Frontend)
parse y = first WParseErrorBundle . runParser (toParsec module' <* eof) y