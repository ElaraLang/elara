module Elara.TypeInfer.Error where

import Elara.AST.Typed
import Elara.Data.Unique (UniqueId)
import Elara.Error (ReportableError (report), writeReport)
import Error.Diagnose (Report (Err))

data TypeError
    = TypeMismatch PartialType PartialType
    | UnboundVariable UniqueId
    deriving (Show)

instance ReportableError TypeError where
    report (TypeMismatch expected actual) =
        writeReport $
            Err Nothing "Type mismatch" [] []
    report (UnboundVariable var) =
        writeReport $
            Err Nothing "Unbound variable" [] []