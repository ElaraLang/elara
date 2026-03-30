{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
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
import Data.Kind qualified as Kind
import Data.Map qualified as M
import Data.Traversable (for)
import Data.Type.Equality ((:~:))
import GHC.Generics (Generic (..), K1 (..), M1 (..), U1 (..), V1, (:*:) (..), (:+:) (..))
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
import Relude qualified (id)
import "generic-optics" Data.Generics.Sum qualified as GenericsSum

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

{- | A generalized 'Plated' type that can traverse children of any type within a structure.

@Plated a s@ means we can find all immediate children of type @a@ within a value of type @s@.
The common case @Plated a a@ is the traditional self-recursive traversal.

The default implementation uses 'gplate' from @optics@ with a compile-time safety check
('SafeGPlate') that ensures no sub-components are silently skipped due to missing 'Generic'
instances.

Instances have to be declared manually for all cases, due to type checking limitations.
-}
class SafeGPlate (Rep s) a => Plated a s where
    plate :: Traversal' s a
    default plate :: (GPlate a s, SafeGPlate (Rep s) a) => Traversal' s a
    plate = gplate

{- | Safe version of 'gplate' for cross-type traversal without needing a 'Plated' instance.

Like 'gplate', this finds all immediate children of type @a@ in a value of type @s@ using
'Generic'. Unlike bare 'gplate', it includes the 'SafeGPlate' compile-time check to ensure
no sub-components are silently skipped.
-}
genericPlate :: forall a s. (GPlate a s, SafeGPlate (Rep s) a) => Traversal' s a
genericPlate = gplate

{- | Verify that all fields in a generic representation are safe for 'gplate' traversal.

Every field type must have a 'Generic' instance so that 'gplate' can recurse into it.
If a field lacks 'Generic', 'gplate' would silently skip it.
This class ensures that every field must have a 'Generic' instance.
-}
class SafeGPlate (f :: Kind.Type -> Kind.Type) a

instance SafeGPlate f a => SafeGPlate (M1 i c f) a

instance (SafeGPlate l a, SafeGPlate r a) => SafeGPlate (l :*: r) a

instance (SafeGPlate l a, SafeGPlate r a) => SafeGPlate (l :+: r) a

instance SafeGPlate U1 a

instance SafeGPlate V1 a

-- | Every field must have a 'Generic' instance
instance Generic f => SafeGPlate (K1 i f) a

cosmos :: Plated a a => Fold a a
cosmos = cosmosOf plate

cosmosOn :: Plated a a => Traversal' a a -> Fold a a
cosmosOn d = cosmosOnOf d plate

cosmosOnOf :: Plated a a => Traversal' a a -> Traversal' a a -> Fold a a
cosmosOnOf d p = d % cosmosOf p

transform :: Plated a a => (a -> a) -> a -> a
transform = transformOf plate

transformOf' :: Is k A_Traversal => Optic k is s a1 a2 b -> (a2 -> b) -> s -> a1
transformOf' optic f a = runIdentity $ traverseOf optic (fmap Identity f) a

concatMapOf :: Is k A_Fold => Optic' k is a1 a2 -> (a2 -> [b]) -> a1 -> [b]
concatMapOf l f = toListOf l >>> concatMap f
