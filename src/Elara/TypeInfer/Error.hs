module Elara.TypeInfer.Error where

import Elara.TypeInfer.Common
import Prelude hiding (Type)
import Elara.Data.Name (Name)

data TypeError
  = UnificationFail Type Type
  | UnificationMismatch [Type] [Type]
  | InfiniteType TypeVariable Type
  | LetDefConflict Type Type
  | UnboundVariable Name TypeEnv
  | Other Text
  deriving (Eq, Ord, Show)