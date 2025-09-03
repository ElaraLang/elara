{-# LANGUAGE TemplateHaskell #-}

module Elara.Data.Unique.Effect where

import Effectful (Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret, reinterpret)
import Effectful.State.Static.Shared
import Effectful.TH (makeEffect)
import Elara.Data.Unique (Unique (..), UniqueId (..), UniqueSupply (..), globalUniqueSupply)
import System.IO.Unsafe (unsafePerformIO)

data UniqueGen :: Effect where
    NewUniqueNum :: UniqueGen m Int

makeEffect ''UniqueGen

uniqueGenToState :: State UniqueSupply :> r => Eff (UniqueGen : r) a -> Eff r a
uniqueGenToState = interpret $ \_ -> \case
    NewUniqueNum -> do
        UniqueSupply us <- get
        case us of
            [] -> error "Ran out of unique numbers"
            (u : us) -> do
                put (UniqueSupply us)
                pure u

-- | Interpret UniqueGen using a provided IORef UniqueSupply.
uniqueGenToIORef :: IORef UniqueSupply -> IOE :> r => Eff (UniqueGen : r) a -> Eff r a
uniqueGenToIORef ref = interpret $ \_ -> \case
    NewUniqueNum -> liftIO $ atomicModifyIORef' ref $ \(UniqueSupply us) ->
        case us of
            [] -> (UniqueSupply [], error "Ran out of unique numbers")
            (u : us') -> (UniqueSupply us', u)

uniqueGenToGlobalIO :: IOE :> r => Eff (UniqueGen : r) a -> Eff r a
uniqueGenToGlobalIO = uniqueGenToIORef globalUniqueSupply

makeUniqueId :: UniqueGen :> r => Eff r UniqueId
makeUniqueId = UniqueId <$> makeUnique ()

makeUnique :: UniqueGen :> r => a -> Eff r (Unique a)
makeUnique a = Unique a <$> newUniqueNum
