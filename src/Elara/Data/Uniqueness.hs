{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Elara.Data.Uniqueness where

import Data.Map qualified as M
import Data.Multimap qualified as Mu

-- Either allows multiple values per key (multimap) or a single value per key (map).
class MapLike m f | m -> f where
  toMap :: m k v -> M.Map k (f v)
  liftSingle :: v -> f v

instance MapLike M.Map Identity where
  toMap = coerce
  liftSingle = Identity

instance MapLike Mu.ListMultimap [] where
  toMap = Mu.toMap
  liftSingle = one

data Uniqueness
  = Unique
  | Many
  deriving (Show, Eq)

type family Structure uniqueness where
  Structure 'Unique = M.Map
  Structure 'Many = Mu.ListMultimap
