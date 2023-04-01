module Elara.TypeInfer.Error where

import Elara.AST.Typed
import Elara.Data.Unique (UniqueId)

data TypeError
    = TypeMismatch PartialType PartialType
    | UnboundVariable UniqueId
    deriving (Show)