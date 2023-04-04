module Elara.TypeInfer.Domain (Domain (..))
where

import Elara.Data.Pretty (Pretty (..))

-- | The domain over which a @forall@ is quantified
data Domain
  = -- | @forall (a : Type) . …@
    Type
  | -- | @forall (a : Fields) . …@
    Fields
  | -- | @forall (a : Alternatives) . …@
    Alternatives
  deriving (Eq, Generic, Show)

instance Pretty Domain where
  pretty Type = "Type"
  pretty Fields = "Fields"
  pretty Alternatives = "Alternatives"