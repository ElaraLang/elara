module Elara.Parse where

import Elara.AST.Module (Module)
import Elara.AST.Select
import Elara.Parse.Error
import Elara.Parse.Module (module')
import Elara.Parse.Primitives (toParsec)
import Text.Megaparsec (MonadParsec (eof), ParseErrorBundle, runParser)

parse :: FilePath -> Text -> Either (ParseErrorBundle Text ElaraParseError) (Module Frontend)
parse = runParser (toParsec module' <* eof)