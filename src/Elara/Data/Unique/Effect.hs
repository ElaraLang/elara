{-# LANGUAGE TemplateHaskell #-}

module Elara.Data.Unique.Effect where

import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret, reinterpret)
import Effectful.State.Static.Shared
import Effectful.TH (makeEffect)
import Elara.Data.Unique (Unique (..), UniqueId (..), UniqueSupply (..))

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

makeUniqueId :: UniqueGen :> r => Eff r UniqueId
makeUniqueId = UniqueId <$> makeUnique ()

makeUnique :: UniqueGen :> r => a -> Eff r (Unique a)
makeUnique a = Unique a <$> newUniqueNum
