module Elara.Query.Errors where

import Effectful
import Effectful.Error.Static
import Elara.AST.Select
import Elara.Desugar.Error
import Elara.Rename.Error
import Elara.Shunt.Error

type family StandardQueryError ast :: [Effect] where
    StandardQueryError Desugared = '[Error DesugarError]
    StandardQueryError Renamed = '[Error RenameError]
    StandardQueryError Shunted = '[Error ShuntError]
