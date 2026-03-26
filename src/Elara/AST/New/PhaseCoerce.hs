{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Type-safe coercion between AST phases where the relevant type families resolve to the same types.
Uses GHC Generics to structurally convert, so it will fail to compile if the phases diverge —
unlike @unsafeCoerce@, this is refactor-safe.
-}
module Elara.AST.New.PhaseCoerce (
    PhaseCoerce (..),
    GPhaseCoerce (..),
)
where

import GHC.Generics (Generic (..), K1 (..), M1 (..), U1 (..), V1, (:*:) (..), (:+:) (..))
import GHC.Generics qualified as G

{- | Coerce a value from one phase to another where the types are structurally identical.
The default implementation uses GHC Generics to convert via the generic representation.
Leaf types that are literally the same between phases get the identity instance.
-}
class PhaseCoerce a b where
    phaseCoerce :: a -> b
    default phaseCoerce :: (Generic a, Generic b, GPhaseCoerce (Rep a) (Rep b)) => a -> b
    phaseCoerce = G.to . gphaseCoerce . G.from

-- | Generic structural coercion between representations
class GPhaseCoerce f g where
    gphaseCoerce :: f x -> g x

instance GPhaseCoerce f g => GPhaseCoerce (M1 i c f) (M1 i' c' g) where
    gphaseCoerce (M1 x) = M1 (gphaseCoerce x)

instance (GPhaseCoerce fl gl, GPhaseCoerce fr gr) => GPhaseCoerce (fl :*: fr) (gl :*: gr) where
    gphaseCoerce (l :*: r) = gphaseCoerce l :*: gphaseCoerce r

instance (GPhaseCoerce fl gl, GPhaseCoerce fr gr) => GPhaseCoerce (fl :+: fr) (gl :+: gr) where
    gphaseCoerce (L1 x) = L1 (gphaseCoerce x)
    gphaseCoerce (R1 x) = R1 (gphaseCoerce x)

instance PhaseCoerce a b => GPhaseCoerce (K1 i a) (K1 i' b) where
    gphaseCoerce (K1 x) = K1 (phaseCoerce x)

instance GPhaseCoerce U1 U1 where
    gphaseCoerce U1 = U1

instance GPhaseCoerce V1 V1 where
    gphaseCoerce v = case v of {}

-- | identity instance
instance {-# INCOHERENT #-} PhaseCoerce a a where
    phaseCoerce = identity

instance {-# INCOHERENT #-} PhaseCoerce a b => PhaseCoerce [a] [b] where
    phaseCoerce = map phaseCoerce

instance {-# INCOHERENT #-} PhaseCoerce a b => PhaseCoerce (Maybe a) (Maybe b) where
    phaseCoerce = fmap phaseCoerce

instance {-# INCOHERENT #-} PhaseCoerce a b => PhaseCoerce (NonEmpty a) (NonEmpty b) where
    phaseCoerce = fmap phaseCoerce

instance {-# INCOHERENT #-} (PhaseCoerce a1 b1, PhaseCoerce a2 b2) => PhaseCoerce (a1, a2) (b1, b2) where
    phaseCoerce (x, y) = (phaseCoerce x, phaseCoerce y)
