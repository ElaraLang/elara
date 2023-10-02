module Elara.TypeInfer.Unique where

import Elara.Data.Unique (Unique, UniqueGen, UniqueId (UniqueId), makeUniqueId)
import Polysemy

type UniqueTyVar =
    Unique
        (Maybe Text) -- Optional name for the type variable. Improves error messages

uniqueIdToTyVar :: UniqueId -> UniqueTyVar
uniqueIdToTyVar (UniqueId c) = fmap (const Nothing) c

makeUniqueTyVar :: (Member UniqueGen r) => Sem r UniqueTyVar
makeUniqueTyVar = uniqueIdToTyVar <$> makeUniqueId

makeUniqueTyVarWith :: (Member UniqueGen r) => Text -> Sem r UniqueTyVar
makeUniqueTyVarWith name = const (Just name) <<$>> makeUniqueTyVar
