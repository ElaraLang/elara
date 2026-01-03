module Elara.Core.LiftClosures.Error where

import Elara.Core qualified as Core
import Elara.Core.Pretty ()
import Elara.Data.Pretty
import Elara.Data.Unique
import Elara.Error (ReportableError)

-- | Errors that can occur during closure lifting
data ClosureLiftError
    = -- | Type variable where term variable expected
      TyVarInTermPosition Core.Var
    | -- | Expected a CExpr but got something else
      ExpectedCExpr Text
    | -- | Body construction failed during lifting
      BodyConstructionFailed Core.Var
    | -- | Expected a function type for closure chain
      ExpectedFunctionType Core.Var Core.Type
    | -- | Failed to determine type of lifted lambda
      CannotDetermineType Core.Var
    | -- | Failed to determine lifted name
      CannotDetermineLiftedName (Unique Text)
    deriving (Show, Typeable, Generic)

instance Exception ClosureLiftError

instance Pretty ClosureLiftError where
    pretty (TyVarInTermPosition v) =
        "Type variable found in term position:" <+> pretty v
    pretty (ExpectedCExpr ctx) =
        "Expected CExpr in" <+> pretty ctx
    pretty (BodyConstructionFailed v) =
        "Failed to construct body for lifted closure:" <+> pretty v
    pretty (ExpectedFunctionType v t) =
        "Expected function type for" <+> pretty v <> ", got:" <+> pretty t
    pretty (CannotDetermineType v) =
        "Cannot determine type of:" <+> pretty v
    pretty (CannotDetermineLiftedName u) =
        "Cannot determine lifted name for unique:" <+> pretty u

instance ReportableError ClosureLiftError
