module Elara.Data.TypeAnnotation where

import Elara.Data.Name (ModuleName, Name)
import Elara.Data.Type (Type)

data TypeAnnotation = TypeAnnotation
  { varName :: Name,
    type' :: Type (Maybe ModuleName)
  }