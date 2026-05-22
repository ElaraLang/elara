module Elara.Query.Errors where

import Effectful
import Effectful.Error.Static
import Effectful.Writer.Static.Local
import Elara.AST.Phases.Desugared (Desugared)
import Elara.AST.Phases.Renamed (Renamed)
import Elara.AST.Phases.Shunted (Shunted)
import Elara.AST.Phases.Typed (Typed)
import Elara.Desugar.Error
import Elara.Error
import Elara.Rename.Error
import Elara.Shunt.Error

type family StandardQueryError ast :: [Effect] where
    StandardQueryError Desugared = '[Error DesugarError, Error ElaraError, Writer [ElaraWarning]]
    StandardQueryError Renamed = '[Error RenameError, Error ElaraError, Writer [ElaraWarning]]
    StandardQueryError Shunted = '[Error ShuntError, Error ElaraError, Writer [ElaraWarning]]
    StandardQueryError Typed = '[Error ElaraError, Writer [ElaraWarning]]
