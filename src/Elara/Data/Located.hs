{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.Data.Located where

import Control.Lens.Extras (uniplate)
import Control.Lens.Plated (Plated (plate))
import Data.Data (Data)
import Data.Kind (Type)

{-
Stores location metadata about where something is in the source code.
Used for nice error messages.
-}

data Located expr = Located Region expr
  deriving (Show, Eq, Traversable, Foldable, Data)

data NoLocated

data IsLocated

type family XRec wrapKind = (wrapper :: Type -> Type) 

type instance XRec NoLocated = Unwrapped

type instance XRec IsLocated = Located


newtype Unwrapped a = Unwrapped a
  deriving (Show, Eq, Data, Functor, Foldable, Traversable)

instance (Data expr, Plated expr) => Plated (Located expr) where
  plate = uniplate

instance Functor Located where
  fmap f (Located region expr) = Located region (f expr)

-- Region in the source code. This is calculated as an offset for efficiency.
data Region = Region
  { startOffset :: Int,
    endOffset :: Int
  }
  deriving (Show, Eq, Data)

located :: Region -> expr -> Located expr
located = Located

getRegion :: Located expr -> Region
getRegion (Located r _) = r

unlocate :: Located expr -> expr
unlocate (Located _ expr) = expr

merge :: (Located a -> Located b -> c) -> Located a -> Located b -> Located c
merge fn l1 l2 =
  Located
    (spanningRegion [getRegion l1, getRegion l2])
    (fn l1 l2)

spanningRegion :: [Region] -> Region
spanningRegion regions =
  Region
    { startOffset = minimum (map startOffset regions),
      endOffset = maximum (map endOffset regions)
    }