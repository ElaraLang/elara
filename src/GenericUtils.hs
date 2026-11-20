{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GenericUtils where

import GHC.Generics
import Optics

import Data.Kind qualified as Kind

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

transform :: Plated a a => (a -> a) -> a -> a
transform = transformOf plate
