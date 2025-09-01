{-# LANGUAGE DefaultSignatures #-}

module Prelude (
    module Relude,
    for,
    (:~:),
    (<<$),
    ($>>),
    (<<&>>),
    insertWithM,
    modifyM,
    identity,
    module Optics,
    module Optics.Operators,
    module Optics.State.Operators,
    module Data.Function,
    Plated (..),
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
import Data.Map qualified as M
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
import Optics.State.Operators ((%=), (?=))
import Polysemy (Member, Sem)
import Polysemy.State (State, get, put)
import Relude hiding (Constraint, Reader, State, Type, ask, evalState, execState, get, gets, id, identity, local, modify, put, runReader, runState)
import Relude qualified (id)

(<<$) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
a <<$ f = fmap (a <$) f

($>>) :: (Functor f, Functor g) => f (g a) -> b -> f (g b)
f $>> a = fmap ($> a) f

(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
f <<&>> a = fmap (a <$>) f

infixl 4 <<$, $>>, <<&>>

insertWithM :: (Ord k, Applicative m) => (a -> a -> m a) -> k -> a -> Map k a -> m (Map k a)
insertWithM f k v m = case M.lookup k m of
    Nothing -> pure (M.insert k v m)
    Just v' -> M.insert k <$> f v v' <*> pure m

modifyM :: Member (State s) r => (s -> Sem r s) -> Sem r ()
modifyM f = get >>= (put <=< f)

identity :: a -> a
identity = Relude.id

-- traversed1 :: Traversable1 f => IndexedTraversal1 Int (f a) (f b) a b

class Plated a where
    plate :: Traversal' a a
    default plate :: GPlate a a => Traversal' a a
    plate = gplate

cosmos :: Plated a => Fold a a
cosmos = cosmosOf plate

cosmosOn d = cosmosOnOf d plate

cosmosOnOf d p = d % cosmosOf p

transform :: Plated a => (a -> a) -> a -> a
transform = transformOf plate

transformOf' :: Is k A_Traversal => Optic k is s a1 a2 b -> (a2 -> b) -> s -> a1
transformOf' optic f a = runIdentity $ traverseOf optic (fmap Identity f) a

concatMapOf :: Is k A_Fold => Optic' k is a1 a2 -> (a2 -> [b]) -> a1 -> [b]
concatMapOf l f = toListOf l >>> concatMap f
