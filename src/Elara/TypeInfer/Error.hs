module Elara.TypeInfer.Error where

import Elara.TypeInfer.Common
import Prelude hiding (Type)

data TypeError
  = UnificationFail Type Type
  | UnificationMismatch [Type] [Type]
  | InfiniteType TypeVariable Type
  | LetDefConflict Type Type
  | UnboundVariable Text
  | Other Text
  deriving (Eq, Ord, Show)