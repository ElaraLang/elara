module Elara.TypeInfer.Environment where

import Data.List (lookup)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Elara.Data.Name (Name)
import Elara.TypeInfer.Common
import Elara.TypeInfer.Substitute
import Prelude hiding (Type)

emptyEnvironment :: TypeEnv
emptyEnvironment = TypeEnv Map.empty

remove :: TypeEnv -> Name -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

closeOver :: Type -> Scheme
closeOver = normalize . generalize emptyEnvironment

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
 where
  as = toList $ ftv t `Set.difference` ftv env

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd vars) (normalizeType body)
 where
  vars = zip (ordNub $ fv body) (map (TV . fromString) letters)

  fv (TypeVar a) = [a]
  fv (a :-> b) = fv a ++ fv b
  fv (TypeConstructorApplication a b) = fv a ++ concatMap fv b
  fv (UserDefinedType _ _) = []

  normalizeType (a :-> b) = normalizeType a :-> normalizeType b
  normalizeType t@(UserDefinedType{}) = t
  normalizeType (TypeConstructorApplication a b) = TypeConstructorApplication (normalizeType a) (normalizeType <$> b)
  normalizeType (TypeVar a) =
    case lookup a vars of
      Just (TV x) -> TypeVar x
      Nothing -> error "type variable not in signature"

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']
