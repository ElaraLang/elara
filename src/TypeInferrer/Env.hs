module TypeInferrer.Env where

import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map as M
import qualified Data.Set as Set

newtype TypeEnv = TypeEnv (M.Map TVar Scheme) deriving (Show)

type Infer a = ExceptT TypeError (State Unique) a

newtype Unique = Unique {count :: Int}

type Subst = M.Map TVar Type

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TVar
  

instance Substitutable Type where
  apply _ (TConcrete a) = TConcrete a
  apply s t@(TVariable a) = M.findWithDefault t a s
  apply s (t1 `TFunc` t2) = apply s t1 `TFunc` apply s t2
  apply s (t1 `TImpureFunc` t2) = apply s t1 `TImpureFunc` apply s t2

  ftv TConcrete {} = Set.empty
  ftv (TVariable a) = Set.singleton a
  ftv (t1 `TFunc` t2) = ftv t1 `Set.union` ftv t2
  ftv (t1 `TImpureFunc` t2) = ftv t1 `Set.union` ftv t2
  

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ M.map (apply s) env
  ftv (TypeEnv env) = ftv $ M.elems env

instance Substitutable Scheme where
  apply s (Forall as t) = Forall as $ apply s' t
    where
      s' = foldr M.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv = foldr (Set.union . ftv) Set.empty

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  | Other String
  deriving (Eq, Ord, Show)

newtype TVar = TV String
  deriving (Eq, Ord)

instance Show TVar where
  show (TV s) = s

data Scheme = Forall [TVar] Type
  deriving (Eq, Ord)

instance Show Scheme where
  show (Forall [] t) = show t
  show (Forall vars t) = "∀" ++ foldr (\v s -> s ++ " " ++ show v) "" vars ++ "." ++ show t

data Type
  = TVariable TVar
  | TConcrete String
  | TFunc Type Type
  | TImpureFunc Type Type
  deriving (Eq, Ord)

instance Show Type where
  show (TVariable t) = show t
  show (TConcrete s) = s
  show (TFunc t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
  show (TImpureFunc t1 t2) = "(" ++ show t1 ++ " => " ++ show t2 ++ ")"




baseEnv :: TypeEnv
baseEnv =
  TypeEnv $
    M.fromList
      [ ( TV "println",
          Forall [TV "a"] (TImpureFunc (TVariable $ TV "a") (TConcrete "()"))
        ),
        ( TV "+",
          Forall [TV "a"] (TFunc (TVariable $ TV "a") (TFunc (TVariable $ TV "a") (TVariable $ TV "a")))
        )
       ]