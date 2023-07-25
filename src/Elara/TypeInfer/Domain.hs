module Elara.TypeInfer.Domain (Domain (..))
where

import Elara.Data.Pretty (Pretty (..))
import Data.Data (Data)

-- | The domain over which a @forall@ is quantified
data Domain
    = -- | @forall (a : Type) . …@
      Type
    | -- | @forall (a : Fields) . …@
      Fields
    | -- | @forall (a : Alternatives) . …@
      Alternatives
    deriving (Eq, Ord, Generic, Show, Data)

instance Pretty Domain where
    pretty Type = "Type"
    pretty Fields = "Fields"
    pretty Alternatives = "Alternatives"
