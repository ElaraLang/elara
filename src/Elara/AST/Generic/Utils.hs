{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Elara.AST.Generic.Utils where

import Data.Kind qualified as Kind
import Elara.AST.Generic.Common
import Elara.AST.Region

{- | When fields may be optional, we need a way of representing that generally. This class does that.
In short, it converts a type to a 'Maybe'. If the type is already a 'Maybe', it is left alone.
If it is not, it is wrapped in a 'Just'. If it is 'NoFieldValue', it is converted to 'Nothing'.
-}
class ToMaybe i o where
    toMaybe :: i -> o

instance {-# OVERLAPPING #-} ToMaybe NoFieldValue (Maybe a) where
    toMaybe _ = Nothing

instance ToMaybe (Maybe a) (Maybe a) where
    toMaybe = identity

instance {-# INCOHERENT #-} ToMaybe a (Maybe a) where
    toMaybe = Just

instance ToMaybe a b => ToMaybe (Located a) b where
    toMaybe = toMaybe . view unlocated

-- | Like 'ToMaybe' but for lists. Useful when fields may be lists or single values.
class ToList i o where
    fieldToList :: i -> o

instance {-# OVERLAPPING #-} ToList NoFieldValue [a] where
    fieldToList _ = []

instance {-# OVERLAPPING #-} ToList DataConCantHappen [a] where
    fieldToList _ = error "fieldToList: DataConCantHappen"

instance ToList [a] [a] where
    fieldToList = identity

instance {-# INCOHERENT #-} ToList a [a] where
    fieldToList = pure

instance ToList a b => ToList (Located a) b where
    fieldToList = fieldToList . view unlocated

{- | Sometimes fields are wrapped in functors eg lists, we need a way of transcending them.
This class does that.
For example, let's say we have `cleanPattern :: Pattern ast1 -> Pattern ast2`, and `x :: Select ast1 "Pattern"`.
`x` could be `Pattern ast1`, `[Pattern ast1]`, `Maybe (Pattern ast1)`, or something else entirely.
`cleanPattern` will only work on the first of these, so we need a way of lifting it to the others.
Obviously, this sounds like a Functor, but `Pattern ast1` has the wrong kind for a functor.
This class half-solves that problem by allowing us to lift a function to a functorish type, whether it is a functor or not.
-}
class ApplyAsFunctorish i o a b where
    applyAsFunctorish :: (a -> b) -> i -> o

instance Functor f => ApplyAsFunctorish (f a) (f b) a b where
    applyAsFunctorish = fmap

instance ApplyAsFunctorish a b a b where
    applyAsFunctorish f = f

instance ApplyAsFunctorish NoFieldValue NoFieldValue a b where
    applyAsFunctorish _ = identity

instance ApplyAsFunctorish DataConCantHappen DataConCantHappen a b where
    applyAsFunctorish _ = error "applyAsFunctorish: DataConCantHappen"

instance ApplyAsFunctorish a b c d => ApplyAsFunctorish (a, a) (b, b) c d where
    applyAsFunctorish f (a, b) = (applyAsFunctorish f a, applyAsFunctorish f b)

class DataConAs a b where
    dataConAs :: a -> b
    asDataCon :: b -> a

instance DataConAs a a where
    dataConAs = identity
    asDataCon = identity

instance DataConAs DataConCantHappen a where
    dataConAs = dataConCantHappen
    asDataCon = error "asDataCon: DataConCantHappen"

-- | Unwraps 1 level of 'Maybe' from a type. Useful when a type family returns Maybe
type family UnwrapMaybe (a :: Kind.Type) = (k :: Kind.Type) where
    UnwrapMaybe (Maybe a) = a
    UnwrapMaybe a = a

-- | Unwraps 1 level of '[]' from a type. Useful when a type family returns []
type family UnwrapList (a :: Kind.Type) = (k :: Kind.Type) where
    UnwrapList [a] = a
    UnwrapList a = a
