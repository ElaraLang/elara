{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elara.TypeInfer.Infer where

import Control.Monad.Except hiding (runExceptT)
import Control.Monad.RWS.Strict (
  MonadWriter (tell),
  RWST,
  runRWST,
 )
import Data.Map qualified as Map
import Elara.AST.Typed (Type (..))
import Elara.Data.Name (
  Name,
  nameValue,
 )
import Elara.TypeInfer.Common
import Elara.TypeInfer.Environment
import Elara.TypeInfer.Error (TypeError (..))
import Elara.TypeInfer.Substitute
import Relude.Unsafe ((!!))
import Prelude hiding (
  Constraint,
  Type,
 )

-- The reader part here isn't very useful but it's a bit more convenient than combining the other 3 monads manually
newtype Infer a = Infer (RWST () [Constraint] InferState (Except TypeError) a)
  deriving (Functor, Applicative, Monad, MonadError TypeError, MonadState InferState, MonadWriter [Constraint])

runInfer :: TypeEnv -> Infer a -> Either TypeError (a, TypeEnv, [Constraint])
runInfer env (Infer m) =
  runExcept $
    (\(res, state, constraints) -> (res, typeEnv state, constraints))
      <$> runRWST m () (InferState 0 env)

instance MonadFail Infer where
  fail = throwError . Other . fromString

toInfer :: Except TypeError a -> Infer a
toInfer = Infer . lift

data InferState = InferState
  { count :: Int
  , typeEnv :: TypeEnv
  }

unify :: Type -> Type -> Infer ()
unify t1 t2 = tell [(t1, t2)]

-- Extend type environment
inEnv :: (Var, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  withModifiedEnv scope m

withModifiedEnv :: (TypeEnv -> TypeEnv) -> Infer a -> Infer a
withModifiedEnv f m = do
  env <- gets typeEnv
  modify (\s -> s{typeEnv = f env})
  a <- m
  modify (\s -> s{typeEnv = env}) -- Restore original environment
  pure a

type Solve a = ExceptT TypeError Identity a

type Unifier = (Subst, [Constraint])

bind :: TypeVariable -> Type -> Solve Subst
bind a@(TV a') t
  | t == TypeVar a' = pure nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = pure (one (a, t))

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = pure nullSubst
unifyMany (t1 : ts1) (t2 : ts2) = do
  su1 <- unifies t1 t2
  su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
  pure (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = pure nullSubst
unifies (TypeVar v) t = TV v `bind` t -- Bind a type variable to a type
unifies t (TypeVar v) = TV v `bind` t -- Bind a type variable to a type
unifies (TypeConstructorApplication t1 t2) (TypeConstructorApplication t3 t4) =
  unifyMany (t1 : t2) (t3 : t4)
unifies (t1 :-> t2) (t3 :-> t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

solver :: Unifier -> Solve Subst
solver (su, cs) = case cs of
  [] -> pure su
  (t1, t2) : cs0 -> do
    su1 <- unifies t1 t2
    solver (su1 `compose` su, apply su1 cs0)

runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ Prelude.runExceptT $ solver st where st = (nullSubst, cs)

lookupEnv :: Name -> Infer Type
lookupEnv x = do
  (TypeEnv env) <- gets typeEnv
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable (nameValue x)
    Just s -> instantiate s

maybeLookupEnv :: Name -> Infer (Maybe Type)
maybeLookupEnv x = do
  (TypeEnv env) <- gets typeEnv
  case Map.lookup x env of
    Nothing -> pure Nothing
    Just s -> Just <$> instantiate s

freshTypeVariable :: Infer Type
freshTypeVariable = do
  s <- get
  put s{count = count s + 1}
  pure $ TypeVar $ fromString (letters !! count s)

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const freshTypeVariable) as
  let s = fromList $ zip as as'
  pure $ apply s t