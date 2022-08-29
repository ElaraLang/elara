{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.Data.Type where

import Data.Data (Data)
import Data.Kind qualified as K (Type)
import Elara.Data.Name
import Elara.Data.Qualifications (MaybeQualified)
import Prelude hiding (Type)

data OrId deriving (Data)

data Concrete deriving (Data)

data family TRec kind :: K.Type -> K.Type -> K.Type

data instance TRec Concrete self qual = Concrete self qual deriving (Show, Eq, Data)

data instance TRec OrId self qual
  = Id Int
  | Type self qual
  deriving (Show, Eq, Data)

type RType p qual = TRec p (AbsType p qual) qual

type ConcreteType qual = RType Concrete qual

type TypeOrId qual = RType OrId qual

data AbsType self qual
  = TypeVar Name
  | Function {_from :: RType self qual, _to :: RType self qual}
  | Unit
  | TypeConstructorApplication {_constructor :: RType self qual, _arg :: RType self qual}
  | UserDefinedType Name

makeConcrete :: self -> TRec Concrete self MaybeQualified
makeConcrete ty = Concrete ty Nothing

qual :: ConcreteType qual -> qual
qual (Concrete _ q) = q

deriving instance (Show q, Show (RType x q)) => Show (AbsType x q)

deriving instance (Eq q, Eq (RType x q)) => Eq (AbsType x q)

deriving instance ((Data x), (Data q), Typeable (RType x q), Data (RType x q)) => Data (AbsType x q)