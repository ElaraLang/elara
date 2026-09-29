{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Prelude (
    module Relude,
    for,
    (:~:),
    (<<$),
    ($>>),
    (<<&>>),
    (?:!),
    insertWithM,
    identity,
    module Optics,
    module Optics.Operators,
    module Optics.State.Operators,
    module Data.Function,
    Plated (..),
    SafeGPlate,
    genericPlate,
    cosmos,
    cosmosOn,
    cosmosOnOf,
    transform,
    transformOf',
    concatMapOf,
    GenericsSum.AsConstructor (..),
    GenericsSum.AsConstructor' (..),
)
where

import Data.Function ((&))
import Data.Traversable (for)
import Data.Type.Equality ((:~:))
import Optics (
    A_Fold,
    A_Setter,
    A_Traversal,
    AffineTraversal,
    AffineTraversal',
    At (..),
    Each (..),
    Fold,
    GPlate (..),
    Getter,
    Is,
    Iso,
    Iso',
    Ixed (..),
    Lens,
    Lens',
    Optic,
    Optic',
    Prism,
    Prism',
    Traversal,
    Traversal',
    castOptic,
    coerced,
    cosmosOf,
    folded,
    ifoldMap,
    ifolded,
    ifor,
    ifor_,
    isn't,
    iso,
    itraverse,
    itraverse_,
    lens,
    lensVL,
    makeFields,
    makeLenses,
    makePrisms,
    over,
    preview,
    prism,
    prism',
    re,
    simple,
    to,
    toListOf,
    transformMOf,
    transformOf,
    traversalVL,
    traverseOf,
    traversed,
    view,
    (%),
    _1,
    _2,
    _3,
    _Empty,
    _Just,
    _Left,
    _Nothing,
    _Right,
 )
import Optics.Operators ((%~), (.~), (?~), (^.), (^..), (^?))
import Optics.State.Operators ((?=))
import Relude hiding (Constraint, Reader, State, Type, ask, evalState, execState, get, gets, id, identity, local, modify, put, runReader, runState)

import Data.Map qualified as M
import Relude qualified (id)
import "generic-optics" Data.Generics.Sum qualified as GenericsSum

import GenericUtils

(<<$) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
a <<$ f = fmap (a <$) f

($>>) :: (Functor f, Functor g) => f (g a) -> b -> f (g b)
f $>> a = fmap ($> a) f

(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
f <<&>> a = fmap (a <$>) f

-- | Effectful version of '(?:)'
(?:!) :: Monad m => m (Maybe b) -> m b -> m b
(?:!) m orElse = do
    m' <- m
    maybe orElse pure m'

infixl 4 <<$, $>>, <<&>>, ?:!

insertWithM :: (Ord k, Applicative m) => (a -> a -> m a) -> k -> a -> Map k a -> m (Map k a)
insertWithM f k v m = case M.lookup k m of
    Nothing -> pure (M.insert k v m)
    Just v' -> M.insert k <$> f v v' <*> pure m

{- | Renamed identity function to avoid name clashes.
I also just prefer the name 'identity' for this function as it's less ambiguous.
-}
identity :: a -> a
identity = Relude.id
