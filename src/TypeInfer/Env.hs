{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeInfer.Env where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.RWS
import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Set as Set

newtype TypeEnv = TypeEnv (M.Map Var Scheme) deriving (Eq)

instance Show TypeEnv where
  show (TypeEnv env) = show (M.toList env)

remove :: TypeEnv -> Var -> TypeEnv
remove (TypeEnv env) var = TypeEnv (M.delete var env)

add :: TypeEnv -> Var -> Scheme -> TypeEnv
add (TypeEnv env) var scheme = TypeEnv (M.insert var scheme env)

-- Even though we don't need the R part, the api is more convenient
newtype Infer a = Infer (RWST () [Constraint] InferState (Except TypeError) a)
  deriving (Functor, Applicative, Monad, MonadError TypeError, MonadState InferState, MonadWriter [Constraint])

instance MonadFail Infer where
  fail = throwError . Other

toInfer :: Except TypeError a -> Infer a
toInfer = Infer . lift

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

type Solve a = ExceptT TypeError Identity a

data InferState = InferState {count :: Int, typeEnv :: TypeEnv}

type Subst = M.Map TVar Type

type Var = String

nullSubst :: Subst
nullSubst = M.empty

emptyUnifier :: Unifier
emptyUnifier = (nullSubst, [])

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TVar -- Finds free type variables of a type

instance Substitutable Type where
  apply _ (TCon a) = TCon a
  apply s t@(TVariable a) = M.findWithDefault t a s
  apply s (t1 `TFunc` t2) = apply s t1 `TFunc` apply s t2
  apply s (TImpure t) = TImpure (apply s t)
  apply s (TConApp a b) = TConApp (apply s a) (apply s b)

  ftv TCon {} = Set.empty
  ftv (TVariable a) = Set.singleton a
  ftv (t1 `TFunc` t2) = ftv t1 `Set.union` ftv t2
  ftv (TImpure t) = ftv t
  ftv (TConApp a b) = ftv a `Set.union` ftv b

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

data TypeError
  = UnificationFail Type Type
  | UnificationMismatch [Type] [Type]
  | InfiniteType TVar Type
  | UnboundVariable String
  | Other String
  deriving (Eq, Ord)

instance Show TypeError where
  show (UnificationFail t1 t2) = "Cannot unify " ++ show t1 ++ " with " ++ show t2
  show (UnificationMismatch ts1 ts2) = "Cannot unify " ++ show ts1 ++ " with " ++ show ts2
  show (InfiniteType t t') = "Infinite type: " ++ show t ++ " = " ++ show t'
  show (UnboundVariable v) = "Unbound variable: " ++ v
  show (Other s) = s

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
  | TCon String -- Type Constructors
  | TConApp Type Type -- Type Constructor Application
  | TFunc Type Type
  | TImpure Type -- An impure function type
  deriving (Eq, Ord)

isImpure :: Type -> Bool
isImpure (TImpure _) = True
isImpure _ = False

impurify :: Type -> Type
impurify = TImpure

purify :: Type -> Type
purify (TImpure (a `TFunc` b)) = purify a `TFunc` purify b
purify (TImpure t) = t
purify t = t

instance Show Type where
  show (TVariable t) = show t
  show (TCon s) = s
  show (TFunc t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
  show (TImpure (TFunc t1 t2)) = "(" ++ show t1 ++ " => " ++ show t2 ++ ")"
  show (TImpure other) = "!" ++ show other
  show (TConApp t1 t2) = show t1 ++ " " ++ show t2

baseEnv :: TypeEnv
baseEnv =
  TypeEnv $
    M.fromList
      [ ( "println",
          Forall [TV "a"] (TImpure (TFunc (TVariable $ TV "a") (TCon "()")))
        )
      ]

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
inEnv (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  modEnv scope m

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

addToEnv :: (Var, Scheme) -> Infer ()
addToEnv (x, sc) = do
  env <- gets typeEnv
  let scope e = remove e x `extend` (x, sc)
  modify $ \s -> s {typeEnv = scope env}

bind :: TVar -> Type -> Solve Subst
bind a t
  | t == TVariable a = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return (M.singleton a t)

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s {count = count s + 1}
  return $ TVariable $ TV (letters !! count s)

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = M.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
  where
    as = Set.toList $ ftv t `Set.difference` ftv env

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normalizeType body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVariable a) = [a]
    fv (TFunc a b) = fv a ++ fv b
    fv (TImpure a) = fv a
    fv (TConApp a b) = fv a ++ fv b
    fv (TCon _) = []

    normalizeType (TFunc a b) = TFunc (normalizeType a) (normalizeType b)
    normalizeType (TImpure a) = TImpure (normalizeType a)
    normalizeType (TCon a) = TCon a
    normalizeType (TConApp a b) = TConApp (normalizeType a) (normalizeType b)
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
unifies (TVariable v) t = v `bind` t
unifies t (TVariable v) = v `bind` t
unifies (TConApp t1 t2) (TConApp t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TFunc t1 t2) (TFunc t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TImpure t1) (TImpure t3) = unifies t1 t3
unifies f (TImpure t) = unifies f t -- a is assignable to !a
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
