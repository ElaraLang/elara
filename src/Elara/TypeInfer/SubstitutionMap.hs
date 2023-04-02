{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.TypeInfer.SubstitutionMap where

import Control.Lens
import Data.Map qualified as Map
import Elara.AST.Module (Module, traverseModule)
import Elara.AST.Select (PartialTyped)
import Elara.AST.Typed
import Elara.Data.Pretty
import Elara.Data.Unique
import Elara.TypeInfer.Error
import Polysemy
import Polysemy.Error

newtype SubstitutionMap = SubstitutionMap (Map UniqueId PartialType)
    deriving (Show, Eq, Ord)

instance Semigroup SubstitutionMap where
    (<>) (SubstitutionMap m1) (SubstitutionMap m2) = SubstitutionMap (m1 <> m2)

instance Monoid SubstitutionMap where
    mempty = SubstitutionMap mempty

insert :: UniqueId -> PartialType -> SubstitutionMap -> SubstitutionMap
insert uid partial (SubstitutionMap m) = case partial of
    Id id' -> case Map.lookup id' m of
        Nothing -> SubstitutionMap $ Map.insert uid partial m
        Just another -> SubstitutionMap $ Map.insert uid another m
    _ -> SubstitutionMap $ Map.insert uid partial m

lookup :: UniqueId -> SubstitutionMap -> Maybe PartialType
lookup uid (SubstitutionMap m) = Map.lookup uid m

class Substitutable a b | a -> b where
    substitute ::
        (HasCallStack) =>
        (Member (Error TypeError) r, Member UniqueGen r) =>
        SubstitutionMap ->
        a ->
        Sem r b

instance Substitutable SubstitutionMap SubstitutionMap where
    substitute sub (SubstitutionMap m) = do
        m' <- Map.traverseWithKey (const $ substitute sub) m
        pure (SubstitutionMap m')

instance {-# OVERLAPPABLE #-} (Traversable f, Substitutable a b) => Substitutable (f a) (f b) where
    substitute sub = traverse (substitute sub)

instance Substitutable (Type' PartialType) (Type' PartialType) where
    substitute sub = traverse (substitute sub)

instance Substitutable PartialType PartialType where
    substitute sub = \case
        Id id -> do
            x <- traverse (substitute sub) (lookup id sub)
            pure (fromMaybe (Id id) x)
        Final t -> pure (Final t)
        Partial t -> Partial <$> substitute sub t

instance Substitutable (Expr PartialType) (Expr PartialType) where
    substitute m = traverseOf _Expr (bitraverse (substitute m) (substitute m))

instance Substitutable (Module PartialTyped) (Module PartialTyped) where
    substitute m = traverseModule (substitute m)

instance Pretty SubstitutionMap where
    pretty (SubstitutionMap m) = pretty m