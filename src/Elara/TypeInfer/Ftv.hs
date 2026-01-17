module Elara.TypeInfer.Ftv where

import Data.Set (difference, member)
import Elara.TypeInfer.Environment (TypeEnvironment (..))
import Elara.TypeInfer.Type (Constraint (..), Monotype (..), Polytype (..), Type (..), TypeVariable (..))
import Elara.TypeInfer.Unique (UniqueTyVar)

class Ftv a where
    ftv :: a -> Set TypeVariable

instance Ftv (Monotype loc) where
    ftv (TypeVar _ tv) = one tv
    ftv (TypeConstructor _ _ ts) = foldMap ftv ts
    ftv (Function _ t1 t2) = ftv t1 <> ftv t2

instance Ftv (Type loc) where
    ftv (Polytype t) = ftv t
    ftv (Lifted t) = ftv t

instance Ftv (Polytype loc) where
    ftv (Forall _ tvs _ t) =
        ftv t
            `difference` fromList (SkolemVar <$> tvs)
            `difference` fromList (UnificationVar <$> tvs)

instance Ftv (TypeEnvironment loc) where
    ftv (TypeEnvironment env) = foldMap ftv env

occurs :: Ftv a => TypeVariable -> a -> Bool
occurs tv a = tv `member` ftv a

class Fuv a where
    fuv :: a -> Set UniqueTyVar

instance Fuv (Monotype loc) where
    fuv (TypeVar _ (SkolemVar _)) = mempty
    fuv (TypeVar _ (UnificationVar tv)) = one tv
    fuv (TypeConstructor _ _ ts) = foldMap fuv ts
    fuv (Function _ t1 t2) = fuv t1 <> fuv t2

instance Fuv (Constraint loc) where
    fuv (Equality _ t1 t2) = fuv t1 <> fuv t2
    fuv EmptyConstraint{} = mempty
    fuv (Conjunction _ a b) = fuv a <> fuv b
