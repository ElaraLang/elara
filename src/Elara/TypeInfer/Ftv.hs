module Elara.TypeInfer.Ftv where

import Data.Set (difference, member)
import Elara.TypeInfer.Type (Monotype (..), Type (..))
import Elara.TypeInfer.Unique

class Ftv a where
    ftv :: a -> Set UniqueTyVar

instance Ftv (Monotype loc) where
    ftv (TypeVar tv) = one tv
    ftv (Scalar _) = mempty
    ftv (TypeConstructor _ ts) = foldMap ftv ts
    ftv (Function t1 t2) = ftv t1 <> ftv t2

instance Ftv (Type loc) where
    ftv (Forall tv _ t) = ftv t `difference` one tv

occurs :: Ftv a => UniqueTyVar -> a -> Bool
occurs tv a = tv `member` ftv a
