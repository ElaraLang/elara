{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}

module Rock.MemoE where

import Data.Dependent.HashMap
import Data.GADT.Compare (GEq)
import Data.Kind (Type)
import Effectful
import Effectful.Concurrent.MVar.Strict (Concurrent, MVar')
import Effectful.Dispatch.Dynamic (interpret, reinterpret)
import Effectful.TH (makeEffect)
import Rock (HideEffects)
import Unsafe.Coerce (unsafeCoerce)

{- | a memoisation effect that's basically just a very restricted IORef
This means that queries can depend on this effect without being able to bring in everything in IO
-}
data Memoise :: Effect where
    GetStartedVar :: (forall es. GEq (f es), forall es a. Hashable (f es a)) => Memoise es (DHashMap (HideEffects f) MVar')
    ModifyStartedVar ::
        forall f es b.
        (DHashMap (HideEffects f) MVar' -> (DHashMap (HideEffects f) MVar', b)) ->
        Memoise es b

type instance DispatchOf Memoise = 'Dynamic

makeEffect ''Memoise

memoiseAsIO ::
    IOE :> es =>
    (forall f. IORef (DHashMap (HideEffects f) MVar')) -> Eff (Memoise : es) a -> Eff es a
memoiseAsIO initialStartedVar = interpret $ \_ -> \case
    GetStartedVar @f -> readIORef (initialStartedVar @f)
    ModifyStartedVar @f f -> atomicModifyIORef' (initialStartedVar @f) f

memoiseRunIO ::
    forall (f :: ([Effect] -> Type -> Type)) (es :: [Effect]) b.
    (forall es. GEq (f es), forall es a. Hashable (f es a)) =>
    IOE :> es =>
    Eff (Memoise : es) b -> Eff es b
memoiseRunIO eff = do
    initialVar <- liftIO $ newIORef (mempty @(DHashMap (HideEffects f) MVar'))
    memoiseAsIO (unsafeCoerce initialVar) eff
