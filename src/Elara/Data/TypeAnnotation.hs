module Elara.Data.TypeAnnotation where

import Elara.Data.Name (ModuleName, Name)
import Elara.Data.Type (ConcreteType)

data TypeAnnotation = TypeAnnotation
  { _varName :: Name,
    _type' :: ConcreteType (Maybe ModuleName)
  }
  deriving (Show)