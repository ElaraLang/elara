{-# LANGUAGE TemplateHaskell #-}

module Elara.Data.Unique.Effect where

import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared
import Effectful.TH (makeEffect)
import Elara.Data.Unique (Unique (..), UniqueId (..), UniqueSupply (..), globalUniqueCounter)

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

{- | Interpret UniqueGen using a global atomic Int counter.
This avoids thunk build-up under CAS contention that occurs
with the lazy-list-based UniqueSupply approach.
-}
uniqueGenToGlobalIO :: IOE :> r => Eff (UniqueGen : r) a -> Eff r a
uniqueGenToGlobalIO = interpret $ \_ -> \case
    NewUniqueNum -> liftIO $ atomicModifyIORef' globalUniqueCounter $ \n ->
        let n' = n + 1 in n' `seq` (n', n)

makeUniqueId :: UniqueGen :> r => Eff r UniqueId
makeUniqueId = UniqueId <$> makeUnique ()

makeUnique :: UniqueGen :> r => a -> Eff r (Unique a)
makeUnique a = Unique a <$> newUniqueNum
