module Prelude (
    module Relude,
    (:~:),
    (<<$),
    ($>>),
    (<<&>>),
    insertWithM,
    modifyM,
    identity,
)
where

import Data.Map qualified as M
import Data.Type.Equality ((:~:))
import Polysemy (Member, Sem)
import Polysemy.State (State, get, put)
import Relude hiding (Reader, State, Type, ask, evalState, execState, get, gets, id, identity, local, modify, put, runReader, runState)
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
