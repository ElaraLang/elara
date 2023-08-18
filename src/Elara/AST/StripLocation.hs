{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.StripLocation where

import Elara.AST.Region (Located (Located), SourceRegion)

class StripLocation a b where
    stripLocation :: a -> b

instance StripLocation a a where
    stripLocation = identity

instance StripLocation (Located a) a where
    stripLocation :: Located a -> a
    stripLocation (Located _ a) = a

instance {-# INCOHERENT #-} StripLocation SourceRegion () where
    stripLocation _ = ()

-- We could provide a general Functor instance but the overlapping tends to cause problems

instance {-# OVERLAPPABLE #-} (StripLocation a a', StripLocation b b') => StripLocation (a, b) (a', b') where
    stripLocation (a, b) = (stripLocation a, stripLocation b)

instance {-# OVERLAPPABLE #-} (StripLocation a a') => StripLocation (Maybe a) (Maybe a') where
    stripLocation = fmap stripLocation

instance {-# OVERLAPPABLE #-} (StripLocation a a') => StripLocation [a] [a'] where
    stripLocation = fmap stripLocation

instance {-# OVERLAPPABLE #-} (StripLocation a a') => StripLocation (NonEmpty a) (NonEmpty a') where
    stripLocation = fmap stripLocation
