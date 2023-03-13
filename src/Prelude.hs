module Prelude (
    module Relude,
    type (~),
    (<<$),
    ($>>),
    (<<<$>>>),
) where

import Relude
import Data.Type.Equality (type (~))

(<<$) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
a <<$ f = fmap (a <$) f

($>>) :: (Functor f, Functor g) => f (g a) -> b -> f (g b)
f $>> a = fmap ($> a) f

(<<<$>>>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<<<$>>>) = fmap . fmap . fmap