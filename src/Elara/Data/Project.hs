module Elara.Data.Project where

import Elara.Data.Name (ModuleName)

data Project fields = Project
  { mainModule :: ModuleName,
    mainFile :: FilePath,
    sourceDirectories :: [FilePath],
    fields :: fields
  }
  deriving (Show, Eq)