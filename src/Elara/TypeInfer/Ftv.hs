module Elara.TypeInfer.Ftv where

import Data.Set (difference, member)
import Elara.TypeInfer.Type (Monotype (..), Type (..), TypeVariable (..))
import Elara.TypeInfer.Unique
import Elara.TypeInfer.Environment (TypeEnvironment (..))

class Ftv a where
    ftv :: a -> Set TypeVariable

instance Ftv (Monotype loc) where
    ftv (TypeVar tv) = one tv
    ftv (Scalar _) = mempty
    ftv (TypeConstructor _ ts) = foldMap ftv ts
    ftv (Function t1 t2) = ftv t1 <> ftv t2

instance Ftv (Type loc) where
    ftv (Forall tv _ t) = ftv t `difference`  fromList (tv)
    ftv (Lifted t) = ftv t

instance Ftv (TypeEnvironment loc) where
    ftv (TypeEnvironment env) = foldMap ftv env

occurs :: Ftv a => TypeVariable -> a -> Bool
occurs tv a = tv `member` ftv a
