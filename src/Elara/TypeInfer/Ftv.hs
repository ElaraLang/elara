module Elara.TypeInfer.Ftv where

import Data.Set (difference, member)
import Elara.TypeInfer.Type (Monotype (..), Type (..), TypeVariable (..), Polytype (..), Constraint (..))
import Elara.TypeInfer.Environment (TypeEnvironment (..))
import Elara.TypeInfer.Unique (UniqueTyVar)

class Ftv a where
    ftv :: a -> Set TypeVariable

instance Ftv (Monotype loc) where
    ftv (TypeVar tv) = one tv
    ftv (Scalar _) = mempty
    ftv (TypeConstructor _ ts) = foldMap ftv ts
    ftv (Function t1 t2) = ftv t1 <> ftv t2

instance Ftv (Type loc) where
    ftv (Polytype t) = ftv t
    ftv (Lifted t) = ftv t

instance Ftv (Polytype loc) where
    ftv (Forall tvs _ t) = ftv t `difference` ( fromList (SkolemVar <$> tvs))

instance Ftv (TypeEnvironment loc) where
    ftv (TypeEnvironment env) = foldMap ftv env

occurs :: Ftv a => TypeVariable -> a -> Bool
occurs tv a = tv `member` ftv a


class Fuv a where
    fuv :: a -> Set UniqueTyVar

instance Fuv (Monotype loc) where
    fuv (TypeVar (SkolemVar _)) = mempty
    fuv (TypeVar (UnificationVar tv)) = one tv
    fuv (Scalar _) = mempty
    fuv (TypeConstructor _ ts) = foldMap fuv ts
    fuv (Function t1 t2) = fuv t1 <> fuv t2

instance Fuv (Constraint loc) where
    fuv (Equality t1 t2) = fuv t1 <> fuv t2
    fuv (EmptyConstraint) = mempty
    fuv (Conjunction a b) = fuv a <> fuv b