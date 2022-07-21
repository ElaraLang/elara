module Elara.Data.TypeAnnotation where

import Elara.Data.Name (Name)
import Elara.Data.Qualifications (MaybeQualified)
import Elara.Data.Type (ConcreteType)

data TypeAnnotation = TypeAnnotation
  { _varName :: Name,
    _type :: ConcreteType MaybeQualified
  }
  deriving (Show)