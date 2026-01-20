{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.StripLocation where

import Elara.AST.Region (Located (Located), SourceRegion)
import Elara.Data.AtLeast2List
import GHC.Generics

class StripLocation a b where
    stripLocation :: a -> b

instance {-# INCOHERENT #-} StripLocation a a where
    stripLocation = identity

instance StripLocation s () where
    stripLocation _ = ()

instance {-# OVERLAPPABLE #-} StripLocation a a' => StripLocation (Located a) a' where
    stripLocation (Located _ a) = stripLocation a

-- | Explicit instance for Located () to avoid overlapping with StripLocation s ()
instance {-# OVERLAPPING #-} StripLocation (Located ()) () where
    stripLocation (Located _ a) = a

instance {-# INCOHERENT #-} StripLocation SourceRegion () where
    stripLocation _ = ()

-- We could provide a general Functor instance but the overlapping tends to cause problems

instance {-# OVERLAPPABLE #-} (StripLocation a a', StripLocation b b') => StripLocation (a, b) (a', b') where
    stripLocation (a, b) = (stripLocation a, stripLocation b)

instance {-# OVERLAPPABLE #-} (StripLocation a a', StripLocation b b', StripLocation c c') => StripLocation (a, b, c) (a', b', c') where
    stripLocation (a, b, c) = (stripLocation a, stripLocation b, stripLocation c)

instance {-# OVERLAPPABLE #-} StripLocation a a' => StripLocation (Maybe a) (Maybe a') where
    stripLocation = fmap stripLocation

instance {-# OVERLAPPABLE #-} StripLocation a a' => StripLocation [a] [a'] where
    stripLocation = fmap stripLocation

instance {-# OVERLAPPABLE #-} StripLocation a a' => StripLocation (AtLeast2List a) (AtLeast2List a') where
    stripLocation = fmap stripLocation

instance {-# OVERLAPPABLE #-} StripLocation a a' => StripLocation (NonEmpty a) (NonEmpty a') where
    stripLocation = fmap stripLocation

class GStripLocation i o where
    gStripLocation :: i p -> o p

instance GStripLocation i o => GStripLocation (M1 _a _b i) (M1 _c _d o) where
    gStripLocation (M1 x) = M1 (gStripLocation x)

instance (GStripLocation l l', GStripLocation r r') => GStripLocation (l :+: r) (l' :+: r') where
    gStripLocation (L1 x) = L1 (gStripLocation x)
    gStripLocation (R1 x) = R1 (gStripLocation x)

instance (GStripLocation a a', GStripLocation b b') => GStripLocation (a :*: b) (a' :*: b') where
    gStripLocation (a :*: b) = gStripLocation a :*: gStripLocation b

instance GStripLocation U1 U1 where
    gStripLocation U1 = U1

instance StripLocation a b => GStripLocation (K1 _i a) (K1 _j b) where
    gStripLocation (K1 x) = K1 (stripLocation x)
