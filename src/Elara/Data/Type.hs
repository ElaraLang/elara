{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.Data.Type where

import Data.Data (Data)
import Elara.AST.Name (TypeName)
import Prelude hiding (Type)

data Type qual
    = TypeVar Text
    | FunctionType (Type qual) (Type qual)
    | UnitType
    | TypeConstructorApplication (Type qual) (Type qual)
    | UserDefinedType (qual TypeName)

deriving instance (Show (qual TypeName)) => Show (Type qual)
deriving instance (Eq (qual TypeName)) => Eq (Type qual)
deriving instance (Typeable qual, Data (qual TypeName)) => Data (Type qual)