{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.StripLocation where

import Elara.AST.Region (Located (Located))

class StripLocation a b | a -> b where
    stripLocation :: a -> b

instance StripLocation (Located a) a where
    stripLocation :: Located a -> a
    stripLocation (Located _ a) = a

-- We could provide a general Functor instance but the overlapping tends to cause problems

instance (StripLocation a a', StripLocation b b') => StripLocation (a, b) (a', b') where
    stripLocation (a, b) = (stripLocation a, stripLocation b)

instance (StripLocation a a') => StripLocation (Maybe a) (Maybe a') where
    stripLocation = fmap stripLocation

instance (StripLocation a a') => StripLocation [a] [a'] where
    stripLocation = fmap stripLocation

instance (StripLocation a a') => StripLocation (NonEmpty a) (NonEmpty a') where
    stripLocation = fmap stripLocation