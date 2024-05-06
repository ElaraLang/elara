module Elara.TypeInfer.Domain (Domain (..)) where

import Data.Aeson (ToJSON)
import Data.Data (Data)
import Elara.Data.Pretty (Pretty (..))

-- | The domain over which a @forall@ is quantified
data Domain
    = -- | @forall (a : Type) . …@
      Type
    deriving (Eq, Ord, Generic, Show, Data)

instance ToJSON Domain

instance Pretty Domain where
    pretty Type = "Type"
