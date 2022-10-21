{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elara.TypeInfer.Infer where

import Control.Monad.Except hiding (runExceptT)
import Control.Monad.RWS.Strict (
  MonadWriter (listen, tell),
  RWST,
  runRWST,
 )
import Data.Map qualified as Map
import Elara.Data.Name (
  Name (..),
 )
import Elara.TypeInfer.Common
import Elara.TypeInfer.Environment
import Elara.TypeInfer.Error (TypeError (..))
import Elara.TypeInfer.Substitute
import Print (debugColored)
import Prelude hiding (
  Constraint,
  Type,
 )

-- The reader part here isn't very useful but it's a bit more convenient than combining the other 3 monads manually
newtype Infer a = Infer (RWST () [Constraint] InferState (Except TypeError) a)
  deriving (Functor, Applicative, Monad, MonadError TypeError, MonadState InferState, MonadWriter [Constraint])

runInfer :: InferState -> Infer a -> Either TypeError (a, InferState, [Constraint])
runInfer env (Infer m) =
  runExcept $
    (\(res, inferState, constraints) -> (res, inferState, constraints))
      <$> runRWST m () env

{- | Infers the type of an expression and returns the expression with the inferred type and the inferred type scheme
 | This makes sure that all the constraints emitted by the expression are solved.
 | This function solves all the constraints immediately so is not suitable for globally scoped declarations.
-}
inferScheme :: (a -> Type) -> Infer a -> Infer (a, Scheme)
inferScheme f inf = do
  env <- gets typeEnv
  (res, constraints) <- listen inf
  subst <- liftEither $ runSolve constraints
  let t1 = apply subst (f res)
      sc = generalize env t1
  pure (res, sc)

infer :: (a -> Type) -> Infer a -> InferState -> Either TypeError (a, InferState, Scheme)
infer f inf env = do
  (a, nextEnv, cs) <- runInfer env inf
  let ty = f a
  subst <- runSolve cs
  pure (a, nextEnv, closeOver $ apply subst ty)

infer' :: Infer Type -> InferState -> Either TypeError (InferState, Scheme)
infer' inf env = do
  (scheme, nextEnv, cs) <- runInfer env inf
  subst <- runSolve cs
  pure (nextEnv, closeOver $ apply subst scheme)



execInfer :: Infer a -> Either TypeError a
execInfer i = (\(res, _, _) -> res) <$> runInfer emptyState i

instance MonadFail Infer where
  fail = throwError . Other . fromString

toInfer :: Except TypeError a -> Infer a
toInfer = Infer . lift

data InferState = InferState
  { typeVariables :: [String]
  , typeEnv :: TypeEnv
  }

emptyState :: InferState
emptyState = InferState letters emptyEnvironment

unify :: Type -> Type -> Infer ()
unify t1 t2 = do
  traceShowM (t1, t2)
  tell [(t1, t2)]

-- | Temporarily extend type environment
inEnv :: (Name, Scheme) -> Infer a -> Infer a
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

addToEnv :: (Name, Scheme) -> Infer ()
addToEnv (x, sc) = do
  env <- gets typeEnv
  let scope e = remove e x `extend` (x, sc)
  modify $ \s -> s{typeEnv = scope env}

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
unifies (TypeConstructorApplication t1 t2) (TypeConstructorApplication t3 t4) = unifyMany (t1 : t2) (t3 : t4)
unifies (t1 :-> t2) (t3 :-> t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

solver :: Unifier -> Solve Subst
solver (su, cs) = case cs of
  [] -> pure su
  (t1, t2) : cs0 -> do
    su1 <- unifies t1 t2
    solver (su1 `compose` su, apply su1 cs0)

runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ Prelude.runExceptT $ solver (nullSubst, cs)

lookupEnv :: Name -> Infer Type
lookupEnv x = do
  (TypeEnv env) <- gets typeEnv

  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable x (TypeEnv env)
    Just s -> instantiate s

maybeLookupEnv :: Name -> Infer (Maybe Type)
maybeLookupEnv x = do
  (TypeEnv env) <- gets typeEnv
  case Map.lookup x env of
    Nothing -> pure Nothing
    Just s -> Just <$> instantiate s

freshTypeVariable :: Infer Type
freshTypeVariable = do
  (tv : others) <- gets typeVariables
  modify (\s -> s{typeVariables = others})
  pure $ TypeVar $ fromString tv

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const freshTypeVariable) as
  let s = fromList $ zip as as'
  pure $ apply s t
