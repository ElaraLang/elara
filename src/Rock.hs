{-# LANGUAGE QuantifiedConstraints #-}
-- Modified version of the Rock (https://hackage.haskell.org/package/rock) library
-- with effectful support added
-- Credit to https://gist.github.com/expipiplus1/cfd5c4fb4a5a40338ccf8642fb3d0f1e for the initial changes,
-- further modifications Copyright (c) Alexander Wood, MIT.
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Rock where

import Data.GADT.Compare (GEq)
import Data.Kind (Type)
import Data.Some
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Static (SideEffects (NoSideEffects), StaticRep, evalStaticRep, getStaticRep)

import Elara.Query.Effects (QueryEffects)
import Prelude hiding (atomicModifyIORef, newIORef, readIORef)

-- * Types

type Rules f = forall a es. ((HasCallStack, QueryEffects es, Rock f :> es) => f a -> Eff es a)

data Rock (f :: Type -> Type) :: Effect
type instance DispatchOf (Rock f) = Static NoSideEffects
newtype instance StaticRep (Rock f) = Rock (Rules f)

runRock :: Rules f -> Eff (Rock f : es) a -> Eff es a
runRock r = evalStaticRep (Rock r)

fetch :: (HasCallStack, QueryEffects es, Rock f :> es) => f a -> Eff es a
fetch key = do
    Rock f <- getStaticRep
    f key

-- * Utils

instance (forall a. Hashable (f a), GEq f) => Hashable (Some f) where
    hashWithSalt s (Some f) = hashWithSalt s f
