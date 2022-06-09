module Elara.Package where

import qualified Elara.String as Es

data Name = Name
  { projectName :: Es.String
  }
  deriving (Show)