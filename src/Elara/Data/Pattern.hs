module Elara.Data.Pattern where

import Elara.Data.Name (Name)

data Pattern
  = NamedPattern Name
  | WildPattern
  deriving (Eq, Ord, Show)
