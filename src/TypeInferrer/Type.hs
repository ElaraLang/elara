module TypeInferrer.Type where

import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Interpreter.AST as A

newtype TVar = TV String
  deriving (Eq, Ord, Show)

data Type
  = TVariable TVar
  | TConcrete String
  | TFunc Type Type
  | TImpureFunc Type Type
  deriving (Eq, Ord, Show)

typeInt :: Type
typeInt = TConcrete "Int"

data Scheme = Forall [TVar] Type
  deriving (Eq, Ord, Show)

newtype Unique = Unique {count :: Int}

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  | Other String
  deriving (Eq, Ord, Show)

newtype TypeEnv = TypeEnv (M.Map TVar Scheme)

type Subst = M.Map TVar Type

extend :: TypeEnv -> (TVar, Scheme) -> TypeEnv
extend (TypeEnv env) (tv, scheme) = TypeEnv $ M.insert tv scheme env

type Infer a = ExceptT TypeError (State Unique) a

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = do
  res <- evalState (runExceptT m) (Unique 0)
  return $ closeOver res

closeOver :: (M.Map TVar Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
  where
    sc = generalize (TypeEnv M.empty) (apply sub ty)

normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (fmap TV letters)

    fv (TVariable a) = [a]
    fv (TFunc a b) = fv a ++ fv b
    fv (TImpureFunc a b) = fv a ++ fv b
    fv (TConcrete _) = []

    normtype (TFunc a b) = TFunc (normtype a) (normtype b)
    normtype (TImpureFunc a b) = TImpureFunc (normtype a) (normtype b)
    normtype (TConcrete a) = TConcrete a
    normtype (TVariable a) =
      case lookup a ord of
        Just x -> TVariable x
        Nothing -> error "type variable not in signature"

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
  where
    as = Set.toList $ ftv t `Set.difference` ftv env

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

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

lookupEnv :: TypeEnv -> TVar -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x =
  case M.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s -> do
      t <- instantiate s
      return (M.empty, t)

infer :: TypeEnv -> A.Expression -> Infer (Subst, Type)
infer env e = case e of
  A.Reference x -> lookupEnv env $ TV $ show x
  A.BindWithBody (A.IdentifierPattern i) e body -> do
    (s1, t1) <- infer env e
    let env' = apply s1 env
        t'  = generalize env' t1
    (s2, t2) <- infer (env' `extend` (TV $ show i, t')) body
    return (s2 `compose` s1, t2)
  other -> throwError $ Other $ "Can't infer type of " ++ show other

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1