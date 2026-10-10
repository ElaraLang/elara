{- | Module for internal compiler errors.
Unlike the stage-based errors, these errors indicate that something has gone wrong internally, eg an invariant has been violated.
-}
module Elara.Error.Internal where

import Elara.AST.Name
import Elara.Data.Pretty
import Elara.Error

data InternalError
    = RequiredDeclNotFound (Qualified Name)
    | DuplicateDeclAfterDesugar ModuleName Name
    | -- | When we parse an annotation at compile time but its arrangement is invalid
      InvalidAnnotationArrangement
    deriving (Eq, Generic, Show)

instance Pretty InternalError

instance Exception InternalError

instance ElaraDiagnostic InternalError where
    diagnosticCode (RequiredDeclNotFound _) = Just requiredDeclNotFound
    diagnosticCode (DuplicateDeclAfterDesugar{}) = Just duplicateDeclAfterDesugar
    diagnosticCode InvalidAnnotationArrangement = Just invalidAnnotationArrangement
