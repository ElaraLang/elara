module Elara.Package where

import Elara.String qualified as Es

newtype Name = Name
  { projectName :: Es.String
  }
  deriving (Show)