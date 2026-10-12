{-# LANGUAGE QuantifiedConstraints #-}
-- Modified version of the Rock (https://hackage.haskell.org/package/rock) library
-- with effectful support added
-- Credit to https://gist.github.com/expipiplus1/cfd5c4fb4a5a40338ccf8642fb3d0f1e for the initial changes,
-- further modifications Copyright (c) Alexander Wood, MIT.
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Rock where

import Data.GADT.Compare (GEq, geq)
import Data.GADT.Show (GShow (gshowsPrec))
import Data.Kind (Type)
import Data.Some
import Data.Typeable
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, Subset, inject, (:>))
import Effectful.Dispatch.Static (SideEffects (NoSideEffects), StaticRep, evalStaticRep, getStaticRep)
import GHC.Show (Show (..), showChar, showParen, showString)
import Unsafe.Coerce (unsafeCoerce)

import Elara.Logging
import Prelude hiding (atomicModifyIORef, newIORef, readIORef)

-- * Types

type Rules f = forall a es. ((HasCallStack, StructuredDebug :> es) => f es a -> Eff es a)

data Rock (f :: [Effect] -> Type -> Type) :: Effect
type instance DispatchOf (Rock f) = Static NoSideEffects
newtype instance StaticRep (Rock f) = Rock (forall a es. (HasCallStack, StructuredDebug :> es) => f es a -> Eff es a)

runRock :: Rules f -> Eff (Rock f : es) a -> Eff es a
runRock r = evalStaticRep (Rock r)

fetch :: (Subset xs es, Rock f :> es, HasCallStack, StructuredDebug :> xs) => f xs a -> Eff es a
fetch key = do
    Rock f <- getStaticRep
    inject (f key)

-- * Utils

{- | A GADT for forgetting the effects required for each key
The GEq and Eq instances will unsafeCoerce away information on the Effects,
please don't rely on it.

This is used for using query keys as map keys
-}
type HideEffects :: ([Effect] -> Type -> Type) -> Type -> Type
data HideEffects f a where
    HideEffects :: forall f b a. f b a -> HideEffects f a

instance (forall es a. Show (f es a)) => GShow (HideEffects f) where
    gshowsPrec prec (HideEffects x) =
        showParen (prec > 10) (showString "HideEffects" . showChar ' ' . showsPrec 11 x)

instance (forall es. GEq (f es)) => GEq (HideEffects f) where
    geq (HideEffects (a :: f es a)) (HideEffects (b :: f fs b)) =
        case geq a (unsafeCoerce b :: f es b) of
            Nothing -> Nothing
            Just Refl -> Just Refl

instance (forall es. Hashable (f es a)) => Eq (HideEffects f a) where
    HideEffects a == HideEffects b = a == unsafeCoerce b

instance (forall es. Hashable (f es a)) => Hashable (HideEffects f a) where
    hashWithSalt s (HideEffects f) = hashWithSalt s f

instance (forall a. Hashable (f a), GEq f) => Hashable (Some f) where
    hashWithSalt s (Some f) = hashWithSalt s f
