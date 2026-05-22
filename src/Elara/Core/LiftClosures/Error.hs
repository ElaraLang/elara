module Elara.Core.LiftClosures.Error where

import Elara.Core qualified as Core
import Elara.Core.Pretty ()
import Elara.Data.Pretty
import Elara.Data.Unique
import Elara.Error
import Elara.Error.Diagnose (toDiagnoseReports)

-- | Errors that can occur during closure lifting
data ClosureLiftError
    = -- | Type variable where term variable expected
      TyVarInTermPosition Core.Var
    | -- | Variable is not bound in the current scope
      VariableNotFound Core.Var
    | -- | Cannot determine the type of a variable
      CannotDetermineType Core.Var
    | -- | Cannot determine the lifted name for a unique
      CannotDetermineLiftedName (Unique Text)
    | -- | Expected a Core expression, but got something else
      ExpectedCExpr Text
    | -- | Expected a function type, but got something else
      ExpectedFunctionType Core.Var Core.Type
    deriving (Show, Typeable)

instance Exception ClosureLiftError

instance Pretty ClosureLiftError where
    pretty = diagnosticMessage

instance ElaraDiagnostic ClosureLiftError where
    diagnosticMessage = \case
        TyVarInTermPosition v ->
            "Type variable where term variable expected:" <+> pretty v
        VariableNotFound v ->
            "Variable not found:" <+> pretty v
        CannotDetermineType v ->
            "Cannot determine type of:" <+> pretty v
        CannotDetermineLiftedName u ->
            "Cannot determine lifted name for unique:" <+> pretty u
        ExpectedCExpr ctx ->
            "Expected Core expression in context:" <+> pretty ctx
        ExpectedFunctionType v t ->
            "Expected function type for variable" <+> pretty v <> ", but got:" <+> pretty t
