module Elara.Error where

import Elara.Data.Module (Declaration, Import (Import))
import Data.Text (Text)

data Error = GenericError Text
  deriving (Ord, Eq, Show)
