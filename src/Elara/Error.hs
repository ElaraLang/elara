module Elara.Error where

import Data.Text (Text)

data Error = GenericError Text
  deriving (Ord, Eq, Show)
