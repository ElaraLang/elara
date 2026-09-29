{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GenericUtils where

import GHC.Generics
import Optics
import Relude (Functor (fmap), Identity (..), concatMap, runIdentity, ($), (>>>))

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
