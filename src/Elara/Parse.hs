module Elara.Parse where

import Elara.AST.Module (Module)
import Elara.AST.Select
import Elara.Parse.Module (module')
import Text.Megaparsec (MonadParsec (eof), ParseErrorBundle, runParser)
import Elara.Parse.Error

parse :: FilePath -> Text -> Either (ParseErrorBundle Text ElaraParseError) (Module Frontend)
parse = runParser (module' <* eof)