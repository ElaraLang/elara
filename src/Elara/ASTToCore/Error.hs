-- | Errors that can occur during the translation from AST to Core.
module Elara.ASTToCore.Error where

import Elara.AST.Module (Module)
import Elara.AST.Select (Typed)
import Elara.Error (ReportableError (report), writeReport)
import Error.Diagnose (Report (Err))

data ASTToCoreError
    = -- | The main module is missing a main function.
      MainModuleMissingMainFunction (Module Typed)

instance ReportableError ASTToCoreError where
    report (MainModuleMissingMainFunction mod) =
        writeReport $
            Err Nothing "The main module is missing a main function. The main module must contain a function called 'main' with no arguments." [] []
