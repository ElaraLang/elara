{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeInfer.Env where

import Control.Monad (replicateM)
import Control.Monad.Except (Except, ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.RWS.Strict (MonadState (get, put), MonadWriter (tell), RWST, gets, modify)
import Control.Monad.Trans (lift)
import Data.Functor.Identity (Identity (runIdentity))
import Data.List (nub)
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text (Text, pack, unpack)
import Elara.String qualified as Es
import GHC.Generics
import Generic.Data (gshowsPrec)
import Print (debugColored)
import TypeInfer.Type

-- TYPE ENVIRONMENT
-- Map of type variables to schemes, built up as the inference process goes

newtype TypeEnv = TypeEnv (M.Map Var Scheme) deriving (Eq)

instance Show TypeEnv where
  show (TypeEnv env) = show (M.toList env)

remove :: TypeEnv -> Var -> TypeEnv
remove (TypeEnv env) var = TypeEnv (M.delete var env)

add :: TypeEnv -> Var -> Scheme -> TypeEnv
add (TypeEnv env) var scheme = TypeEnv (M.insert var scheme env)

emptyEnv :: TypeEnv
emptyEnv = TypeEnv M.empty

closeOver :: Type -> Scheme
closeOver = normalize . generalize emptyEnv

extend :: TypeEnv -> (Var, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ M.insert x s env

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

uni :: Type -> Type -> Infer ()
uni t1 t2 = tell [(t1, t2)]

-- Extend type environment
inEnv :: (Var, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = withCopyOfEnv $ do
  addToEnv (x, sc)
  m

addToEnv :: (Var, Scheme) -> Infer ()
addToEnv (x, sc) = do
  env <- gets typeEnv
  let scope e = remove e x `extend` (x, sc)
  modify $ \s -> s {typeEnv = scope env}

-- Extend type environment temporarily, returning the result of the computation in the modified environment
modEnv :: (TypeEnv -> TypeEnv) -> Infer a -> Infer a
modEnv f m = do
  env <- gets typeEnv
  modify (\s -> s {typeEnv = f env})
  a <- m
  modify (\s -> s {typeEnv = env}) -- Restore original environment
  return a

withCopyOfEnv :: Infer a -> Infer a
withCopyOfEnv m = do
  env <- gets typeEnv
  a <- m
  modify $ \s -> s {typeEnv = env}
  return a

bind :: TVar -> Type -> Solve Subst
bind a t
  | t == TVariable a = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return (M.singleton a t)

letters :: [String] -- Infinite list of letters for generating fresh type variables
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

freshTVar :: Infer Type -- Generate a fresh type variable
freshTVar = do
  s <- get

  put s {tVars = tail $ tVars s}
  return $ TVariable $ TV $ pack (head $ tVars s) -- the !! is not very efficient here

-- SCHEME OPERATIONS
instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const freshTVar) as
  let s = M.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
  where
    as = Set.toList $ ftv t `Set.difference` ftv env

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normalizeType body)
  where
    ord = zip (nub $ fv body) (map (TV . pack) letters) -- TODO possible performance issue, nub is not very efficient. Maybe use a set instead?
    fv (TVariable a) = [a]
    fv (TFunc a b) = fv a ++ fv b
    fv (TApp a b) = fv a ++ fv b
    fv (TCon _) = []

    normalizeType (TFunc a b) = TFunc (normalizeType a) (normalizeType b)
    normalizeType (TCon a) = TCon a
    normalizeType (TApp a b) = TApp (normalizeType a) (normalizeType b)
    normalizeType (TVariable a) =
      case lookup a ord of
        Just x -> TVariable x
        Nothing -> error "type variable not in signature"

lookupEnv :: Var -> Infer Type
lookupEnv x = do
  (TypeEnv env) <- gets typeEnv
  case M.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s -> instantiate s

maybeLookupEnv :: Var -> Infer (Maybe Type)
maybeLookupEnv x = do
  (TypeEnv env) <- gets typeEnv
  case M.lookup x env of
    Nothing -> return Nothing
    Just s -> Just <$> instantiate s

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return nullSubst
unifyMany (t1 : ts1) (t2 : ts2) =
  do
    su1 <- unifies t1 t2
    su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
    return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return nullSubst
unifies (TVariable v) t = v `bind` t -- Bind a type variable to a type
unifies t (TVariable v) = v `bind` t -- Bind a type variable to a type
unifies (TApp t1 t2) (TApp t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TFunc t1 t2) (TFunc t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

solver :: Unifier -> Solve Subst
solver (su, cs) = case cs of
  [] -> return su
  (t1, t2) : cs0 -> do
    su1 <- unifies t1 t2
    solver (su1 `compose` su, apply su1 cs0)

runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver st
  where
    st = (nullSubst, cs)

-- Even though we don't need the R part, the api is more convenient rather than mixing the other 3 monads
newtype Infer a = Infer (RWST () [Constraint] InferState (Except TypeError) a)
  deriving (Functor, Applicative, Monad, MonadError TypeError, MonadState InferState, MonadWriter [Constraint])

instance MonadFail Infer where
  fail = throwError . Other . Es.fromString

toInfer :: Except TypeError a -> Infer a
toInfer = Infer . lift

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

type Solve a = ExceptT TypeError Identity a

data InferState = InferState
  { tVars :: [String], -- How many fresh type variables have been generated
    typeEnv :: TypeEnv -- The current type environment
  }
  deriving (Show)

emptyInferState :: TypeEnv -> InferState
emptyInferState env = InferState {tVars = letters, typeEnv = env}

type Subst = M.Map TVar Type

type Var = Es.String

nullSubst :: Subst
nullSubst = M.empty

emptyUnifier :: Unifier
emptyUnifier = (nullSubst, [])

data TypeError
  = UnificationFail Type Type
  | ImpurityMismatch Type Type
  | UnificationMismatch [Type] [Type]
  | InfiniteType TVar Type
  | LetDefConflict Type Type
  | UnboundVariable Es.String
  | Other Es.String
  deriving (Eq, Ord, Generic)

instance Show TypeError where
  showsPrec _ (UnificationFail a b) = (("Cannot unify types (" ++ show a ++ ") and (" ++ show b ++ ")") ++)
  showsPrec n x = gshowsPrec n x

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TVar -- Finds free type variables of a type

instance Substitutable Type where
  apply _ (TCon a) = TCon a
  apply s t@(TVariable a) = M.findWithDefault t a s
  apply s (t1 `TFunc` t2) = apply s t1 `TFunc` apply s t2
  apply s (TApp a b) = TApp (apply s a) (apply s b)

  ftv TCon {} = Set.empty
  ftv (TVariable a) = Set.singleton a
  ftv (t1 `TFunc` t2) = ftv t1 `Set.union` ftv t2
  ftv (TApp a b) = ftv a `Set.union` ftv b

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ M.map (apply s) env
  ftv (TypeEnv env) = ftv $ M.elems env

instance Substitutable Scheme where
  apply s (Forall as t) = Forall as $ apply s' t
    where
      s' = foldr M.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
  apply s (t1, t2) = (apply s t1, apply s t2)
  ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv = foldr (Set.union . ftv) Set.empty
