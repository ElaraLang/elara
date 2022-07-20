module Elara.Data.Type where

import Elara.Data.Name

data TypeOrId qualified
  = Id Int
  | Type (Type qualified)
  deriving (Show)

data AbsType ty qual
  = TypeVar Name
  | Function {from :: ty qual, to :: ty qual}
  | Int
  | Float
  | Bool
  | Char
  | String
  | Unit
  | UserDefinedType
      { qualified :: qual,
        name :: Name,
        args :: [ty qual]
      }
  deriving (Show)

type Type = AbsType TypeOrId

newtype ConcreteType qual = ConcreteType (ConcreteAbs qual)
  deriving (Show)

type ConcreteAbs = AbsType ConcreteType

makeConcrete :: ConcreteAbs qual -> ConcreteType qual
makeConcrete = ConcreteType