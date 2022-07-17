module Elara.Data.TypeAnnotation where

import Elara.Data.Name (ModuleName, Name)
import Elara.Data.Type (Type)

data TypeAnnotation = TypeAnnotation
  { _varName :: Name,
    _type' :: Type (Maybe ModuleName)
  }
  deriving (Show)