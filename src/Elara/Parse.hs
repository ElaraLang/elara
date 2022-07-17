module Elara.Parse where

import Elara.AST.Frontend qualified as Frontend
import Elara.Data.Module (Module)
import Elara.Data.TypeAnnotation (TypeAnnotation)
import Elara.Error (Error)
import Elara.Data.Name (ModuleName)

parse :: String -> Either Error (Module Frontend.LocatedExpr TypeAnnotation (Maybe ModuleName))
parse = undefined