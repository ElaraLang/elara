{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.Data.Type where

import Data.Data (Data, Typeable)
import Data.Kind qualified as K (Type)
import Elara.Data.Name
import Elara.Data.Qualifications (MaybeQualified)
import Prelude hiding (Type)

data OrId deriving (Data)

data Concrete deriving (Data)

type family TRec typeKind = (typeWrapper :: K.Type -> K.Type -> K.Type) | typeWrapper -> typeKind

type instance TRec OrId = TypeOrId'

type instance TRec Concrete = ConcreteType'

data TypeOrId' self qual
  = Id Int
  | Type self qual
  deriving (Show, Eq, Data)

-- Basically the identity type but with 2 parameters
data ConcreteType' self qual = Concrete self qual deriving (Show, Eq, Data)

type RType p qual = TRec p (AbsType p qual) qual

type ConcreteType qual = RType Concrete qual

type TypeOrId qual = RType OrId qual

data AbsType self qual
  = TypeVar Name
  | Function {_from :: RType self qual, _to :: RType self qual}
  | Int
  | Float
  | Bool
  | Char
  | String
  | Unit
  | UserDefinedType
      { _qualified :: RType self qual,
        _name :: Name,
        _args :: [RType self qual]
      }

makeConcrete :: self -> ConcreteType' self MaybeQualified
makeConcrete ty = Concrete ty Nothing


deriving instance (Show (RType x q)) => Show (AbsType x q)

deriving instance (Eq (RType x q)) => Eq (AbsType x q)

deriving instance ((Data x), (Data q), Typeable (RType x q), Data (RType x q)) => Data (AbsType x q)