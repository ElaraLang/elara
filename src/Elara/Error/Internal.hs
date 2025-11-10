{- | Module for internal compiler errors.
Unlike the stage-based errors, these errors indicate that something has gone wrong internally, eg an invariant has been violated.
-}
module Elara.Error.Internal where

import Elara.AST.Name

data InternalError
    = RequiredDeclNotFound (Qualified Name)
    | DuplicateDeclAfterDesugar ModuleName Name
    deriving (Show, Eq)

instance Exception InternalError
