{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.Data.Located where

import Control.Lens.Extras (uniplate)
import Control.Lens.Plated (Plated (plate))
import Data.Data (Data)
import Relude.Extra (Foldable1 (..))
import Text.Show

{-
Stores location metadata about where something is in the source code.
Used for nice error messages.
-}

data Located expr = Located Region expr
  deriving (Show, Eq, Ord, Traversable, Foldable, Data)

data NoLocated

data IsLocated

newtype TypeIdentity x = TypeIdentity x
  deriving (Eq, Ord, Data, Functor, Foldable, Traversable)

instance (Show x) => Show (TypeIdentity x) where
  show (TypeIdentity x) = Text.Show.show x

type family XRec wrapKind = (wrapper :: Type -> Type) | wrapper -> wrapKind where
  XRec NoLocated = TypeIdentity
  XRec IsLocated = Located

type family UnIdentity x where
  UnIdentity (TypeIdentity x) = x
  UnIdentity x = x

instance (Data expr, Plated expr) => Plated (Located expr) where
  plate = uniplate

instance Functor Located where
  fmap f (Located region expr) = Located region (f expr)

-- Region in the source code. This is calculated as an offset for efficiency.
data Region = Region
  { startOffset :: Int,
    endOffset :: Int
  }
  deriving (Show, Ord, Eq, Data)

located :: Region -> expr -> Located expr
located = Located

getRegion :: Located expr -> Region
getRegion (Located r _) = r

unlocate :: Located expr -> expr
unlocate (Located _ expr) = expr

merge :: (Located a -> Located b -> c) -> Located a -> Located b -> Located c
merge fn l1 l2 =
  Located
    (spanningRegion (getRegion l1 :| [getRegion l2]))
    (fn l1 l2)

spanningRegion :: NonEmpty Region -> Region
spanningRegion regions =
  Region
    { startOffset = minimum1 (startOffset <$> regions),
      endOffset = maximum1 (endOffset <$> regions)
    }