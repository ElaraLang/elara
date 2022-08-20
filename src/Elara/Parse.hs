module Elara.Parse where

import Data.Text (Text)
import Elara.AST.Frontend qualified as Frontend
import Elara.Data.Module (Module)
import Elara.Data.Name (ModuleName)
import Elara.Data.TypeAnnotation (TypeAnnotation)
import Elara.Data.Uniqueness
import Elara.Error (Error)
import Elara.Parse.Module (module')
import Text.Megaparsec (MonadParsec (eof), ParseErrorBundle, runParser)

parse :: FilePath -> Text -> Either (ParseErrorBundle Text Error) (Module Frontend.LocatedExpr Frontend.Pattern TypeAnnotation (Maybe ModuleName) Many)
parse = runParser (module' <* eof)