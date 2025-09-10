module Elara.TypeInfer.Unique where

import Effectful
import Elara.Data.Unique (Unique, UniqueId (UniqueId))
import Elara.Data.Unique.Effect

type UniqueTyVar =
    Unique (Maybe Text) -- Optional name for the type variable. Improves error messages

uniqueIdToTyVar :: UniqueId -> UniqueTyVar
uniqueIdToTyVar (UniqueId c) = fmap (const Nothing) c

makeUniqueTyVar :: UniqueGen :> r => Eff r UniqueTyVar
makeUniqueTyVar = uniqueIdToTyVar <$> makeUniqueId

makeUniqueTyVarWith :: UniqueGen :> r => Text -> Eff r UniqueTyVar
makeUniqueTyVarWith name = const (Just name) <<$>> makeUniqueTyVar
