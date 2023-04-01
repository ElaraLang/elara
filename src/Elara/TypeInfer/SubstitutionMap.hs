{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.TypeInfer.SubstitutionMap where

import Control.Lens
import Data.Map qualified as Map
import Elara.AST.Typed
import Elara.Data.Unique
import Elara.TypeInfer.Error
import Elara.TypeInfer.TypeVariables
import Polysemy
import Polysemy.Error
import Elara.Data.Unique

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
    substitute ::HasCallStack => (Member (Error TypeError) r, Member UniqueGen r)=> SubstitutionMap -> a -> Sem r b

instance {-# OVERLAPPABLE #-} (Traversable f, Substitutable a b) => Substitutable (f a) (f b) where
    substitute sub = traverse (substitute sub)

instance Substitutable (Type' PartialType) (Type' Type) where
    substitute sub = traverse (substitute sub)

instance Substitutable PartialType Type where
    substitute sub = \case
        Id id -> case lookup id sub of
            Nothing -> Type . TypeVar <$> newTypeVar
            Just partial -> substitute sub partial
        Final t -> pure t
        Partial t -> Type <$> substitute sub t

instance Substitutable (Expr PartialType) (Expr Type) where
    substitute m = traverseOf _Expr (bitraverse (substitute m) (substitute m))