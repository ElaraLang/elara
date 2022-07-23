{-# LANGUAGE DeriveDataTypeable #-}

module Elara.Data.Type where

import Data.Data
import Elara.Data.Name

data TypeOrId qualified
  = Id Int
  | Type (Type qualified)
  deriving (Show, Eq, Data)

data AbsType ty qual
  = TypeVar Name
  | Function {_from :: ty qual, _to :: ty qual}
  | Int
  | Float
  | Bool
  | Char
  | String
  | Unit
  | UserDefinedType
      { _qualified :: qual,
        _name :: Name,
        _args :: [ty qual]
      }
  deriving (Show, Eq, Data)

type Type = AbsType TypeOrId

newtype ConcreteType qual = ConcreteType (ConcreteAbs qual)
  deriving (Show, Eq)

type ConcreteAbs = AbsType ConcreteType

makeConcrete :: ConcreteAbs qual -> ConcreteType qual
makeConcrete = ConcreteType

unwrapType :: ConcreteType qual -> ConcreteAbs qual
unwrapType (ConcreteType t) = t