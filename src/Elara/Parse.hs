module Elara.Parse where

import Elara.AST.Module (Module)
import Elara.AST.Select
import Elara.Parse.Module (module')
import Text.Megaparsec (MonadParsec (eof), ParseErrorBundle, runParser)
import Error.Diagnose.Compat.Megaparsec

type Error = Void

parse :: FilePath -> Text -> Either (ParseErrorBundle Text Error) (Module Frontend)
parse = runParser (module' <* eof)

instance HasHints Error msg where
    hints _ = []