module Elara.Data.Type where

import Elara.Data.Name

data TypeOrId qualified
  = Id Int
  | Type (Type qualified)
  deriving (Show)

data Type qual
  = TypeVar Name
  | Function {from :: TypeOrId qual, to :: TypeOrId qual}
  | Int
  | Float
  | Bool
  | Char
  | String
  | Unit
  | UserDefinedType
      { qualified :: qual,
        name :: Name,
        args :: [TypeOrId qual]
      }
  deriving (Show)