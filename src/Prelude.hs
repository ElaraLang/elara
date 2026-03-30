{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
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
    SafePlated,
    cosmos,
    cosmosOn,
    cosmosOnOf,
    transform,
    transformOf',
    concatMapOf,
    AsConstructor (..),
    AsConstructor' (..),
)
where

import Data.Function ((&))
import Data.Generics.Sum
import Data.Kind qualified as Kind
import Data.Map qualified as M
import Data.Traversable (for)
import Data.Type.Equality ((:~:))
import GHC.Generics (Generic (..), K1 (..), M1 (..), U1 (..), V1, (:*:) (..), (:+:) (..))
import GHC.TypeLits (ErrorMessage (..), TypeError)
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

-- | A 'Plated' type is one that has children of the same type that can be traversed recursively
class Plated a where
    plate :: Traversal' a a
    default plate :: (Generic a, GPlate a a, SafePlated a) => Traversal' a a
    plate = gplate

{- | A type that can be proven to be safe for 'gplate' to traverse, meaning that any field that can contain the type itself is also 'Generic' so 'gplate' can look inside it.
This prevents silent failures where 'gplate' would skip fields of a type that contains itself because it doesn't know how to look inside it.
-}
class SafePlated a

-- | Generic representation of a type is safe for 'gplate' to traverse if all of its fields are safe
instance (Generic a, SafeGPlate (Rep a) a) => SafePlated a

{- | Type class to verify that all fields in a generic representation are "safe" for 'gplate'.
'safe' means that if a field type 'f' can contain 'a', then 'f' must be 'Generic' so 'gplate' can look inside it.
-}
class SafeGPlate (f :: Kind.Type -> Kind.Type) a

instance SafeGPlate f a => SafeGPlate (M1 i c f) a

instance (SafeGPlate l a, SafeGPlate r a) => SafeGPlate (l :*: r) a

instance (SafeGPlate l a, SafeGPlate r a) => SafeGPlate (l :+: r) a

instance SafeGPlate U1 a

instance SafeGPlate V1 a

instance SafeGPlateField f a (CanContain f a) => SafeGPlate (K1 i f) a

{- | Whether type 'f' can potentially contain type 'a'.
We use this to enforce that if 'f' can contain 'a', 'f' must be 'Generic' so 'gplate' can look inside it.
This prevents silent failures where 'gplate' would skip fields of type 'f' that contain 'a' because it doesn't know how to look inside 'f'.
-}
type family CanContain f a :: Bool where
    CanContain a a = 'True
    CanContain Int _ = 'False
    CanContain Integer _ = 'False
    CanContain Float _ = 'False
    CanContain Double _ = 'False
    CanContain Char _ = 'False
    CanContain Text _ = 'False
    CanContain Bool _ = 'False
    CanContain ByteString _ = 'False
    CanContain (Maybe f) a = CanContain f a
    CanContain [f] a = CanContain f a
    CanContain (NonEmpty f) a = CanContain f a
    CanContain (f, g) a = CanContain f a || CanContain g a
    CanContain (f, g, h) a = CanContain f a || CanContain g a || CanContain h a
    -- Default to True for unknown types to be safe
    CanContain () _ = 'False
    CanContain _ _ = 'True

-- | Type-level boolean OR
type family (||) (a :: Bool) (b :: Bool) :: Bool where
    'True || _ = 'True
    'False || b = b

-- | Enforce that if 'f' can contain 'a', it must be 'Generic'
class SafeGPlateField f a (canContain :: Bool)

-- | If 'f' cannot contain 'a', it's safe regardless of whether 'f' is 'Generic'
instance SafeGPlateField f a 'False

-- | If 'f' can contain 'a', it must be 'Generic' so 'gplate' can look inside it
instance (Generic f, GPlate a f) => SafeGPlateField f a 'True

-- | A specialised error message for when a type is missing a Generic instance
instance
    {-# OVERLAPPABLE #-}
    TypeError
        ( 'Text "Type "
            ':<>: 'ShowType f
            ':<>: 'Text " is used in a Plated instance for "
            ':<>: 'ShowType a
            ':$$: 'Text "but "
            ':<>: 'ShowType f
            ':<>: 'Text " does not have a Generic instance."
            ':$$: 'Text "This would cause it to be silently ignored during traversal."
        ) =>
    SafeGPlateField f a 'True

cosmos :: Plated a => Fold a a
cosmos = cosmosOf plate

cosmosOn :: Plated a => Traversal' a a -> Fold a a
cosmosOn d = cosmosOnOf d plate

cosmosOnOf :: Plated a => Traversal' a a -> Traversal' a a -> Fold a a
cosmosOnOf d p = d % cosmosOf p

transform :: Plated a => (a -> a) -> a -> a
transform = transformOf plate

transformOf' :: Is k A_Traversal => Optic k is s a1 a2 b -> (a2 -> b) -> s -> a1
transformOf' optic f a = runIdentity $ traverseOf optic (fmap Identity f) a

concatMapOf :: Is k A_Fold => Optic' k is a1 a2 -> (a2 -> [b]) -> a1 -> [b]
concatMapOf l f = toListOf l >>> concatMap f
