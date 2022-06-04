{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeCheck.Env where

import Control.Monad.Except (Except, ExceptT, MonadError (throwError))
import Control.Monad.RWS.Strict (MonadState, MonadWriter, RWST)
import Data.Functor.Identity (Identity)
import Data.Map.Strict qualified as M
import Data.Set qualified as Set

newtype TypeEnv = TypeEnv (M.Map Var Scheme) deriving (Eq)

instance Show TypeEnv where
  show (TypeEnv env) = show (M.toList env)

remove :: TypeEnv -> Var -> TypeEnv
remove (TypeEnv env) var = TypeEnv (M.delete var env)

add :: TypeEnv -> Var -> Scheme -> TypeEnv
add (TypeEnv env) var scheme = TypeEnv (M.insert var scheme env)

-- Even though we don't need the R part, the api is more convenient rather than mixing the other 3 monads
newtype Infer a = Infer (RWST () [Constraint] InferState (Except TypeError) a)
  deriving (Functor, Applicative, Monad, MonadError TypeError, MonadState InferState, MonadWriter [Constraint])

instance MonadFail Infer where
  fail = throwError . Other

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

type Solve a = ExceptT TypeError Identity a

data InferState = InferState {count :: Int, typeEnv :: TypeEnv}

type Subst = M.Map TVar Type

type Var = String

newtype TVar = TV String
  deriving (Eq, Ord)

instance Show TVar where
  show (TV s) = s

data Scheme = Forall [TVar] Type
  deriving (Eq, Ord)

instance Show Scheme where
  show (Forall [] t) = show t
  show (Forall vars t) = "forall " ++ unwords (map show vars) ++ ". " ++ show t

data Type
  = TVariable TVar
  | TCon String -- Type constructor
  | TApp Type Type -- Type constructor application
  | TFunc Type Type -- Function type
  deriving (Eq, Ord)

instance Show Type where
  show (TVariable t) = show t
  show (TCon s) = s
  show (TApp t1 t2) = show t1 ++ " " ++ show t2
  show (TFunc t1 t2) = show t1 ++ " -> " ++ show t2

data TypeError
  = UnificationFail Type Type
  | ImpurityMismatch Type Type
  | UnificationMismatch [Type] [Type]
  | InfiniteType TVar Type
  | LetDefConflict Type Type
  | UnboundVariable String
  | Other String
  deriving (Eq, Ord)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TVar -- Finds free type variables of a type