{-# LANGUAGE FlexibleInstances #-}

module Elara.TypeInfer.Substitute where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Elara.TypeInfer.Common
import Prelude hiding (Constraint, Type)

class Substitutable a where
  apply :: Subst -> a -> a
  -- ^ Returns all the free type variables of the element

  ftv :: a -> Set.Set TypeVariable

instance Substitutable Type where
  apply _ t@UserDefinedType{} = t
  apply s t@(TypeVar a) = Map.findWithDefault t (TV a) s
  apply s (t1 :-> t2) = apply s t1 :-> apply s t2
  apply s (TypeConstructorApplication a b) = TypeConstructorApplication (apply s a) (apply s b)

  ftv UserDefinedType{} = Set.empty
  ftv (TypeVar a) = one (TV a)
  ftv (t1 :-> t2) = ftv t1 `Set.union` ftv t2
  ftv (TypeConstructorApplication a b) = ftv a `Set.union` ftv b

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env


instance Substitutable Scheme where
  apply s (Forall as t) = Forall as $ apply s' t
   where
    s' = foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
  apply s (t1, t2) = (apply s t1, apply s t2)
  ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv = foldr (Set.union . ftv) Set.empty
occursCheck :: Substitutable a => TypeVariable -> a -> Bool
occursCheck a t = a `Set.member` ftv t