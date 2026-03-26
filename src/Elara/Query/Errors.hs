module Elara.Query.Errors where

import Effectful
import Effectful.Error.Static
import Elara.AST.New.Phases.Desugared (Desugared)
import Elara.AST.New.Phases.Renamed (Renamed)
import Elara.AST.New.Phases.Shunted (Shunted)
import Elara.Desugar.Error
import Elara.Rename.Error
import Elara.Shunt.Error

type family StandardQueryError ast :: [Effect] where
    StandardQueryError Desugared = '[Error DesugarError]
    StandardQueryError Renamed = '[Error RenameError]
    StandardQueryError Shunted = '[Error ShuntError]
