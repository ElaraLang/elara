module TypeInferrer.Type where

import Control.Monad (replicateM)
import Control.Monad.Except
import Control.Monad.State
import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Set as Set
import Debug.Trace (trace, traceId, traceShowId, traceShowM)
import qualified Interpreter.AST as A
import TypeInferrer.Env

typeInt :: Type
typeInt = TConcrete "Int"

extend :: TypeEnv -> (TVar, Scheme) -> TypeEnv
extend (TypeEnv env) (tv, scheme) = TypeEnv $ M.insert tv scheme env

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

-- Generates names for type variables
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

lookupEnv :: TypeEnv -> TVar -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x =
  case M.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s -> do
      t <- instantiate s
      return (M.empty, t)

infer :: TypeEnv -> A.Expression -> Infer (Subst, Type)
infer env ex = case ex of
  A.Constant (A.IntC _) -> return (M.empty, typeInt)
  A.Reference x -> lookupEnv env $ TV $ show x
  A.Lambda param body -> do
    tv <- fresh
    let env' = env `extend` (TV $ show param, Forall [] tv)
    (s1, t1) <- infer env' body
    return (s1, apply s1 tv `TFunc` t1)
  A.BindWithBody (A.IdentifierPattern i) e body -> do
    (s1, t1) <- infer env e
    let env' = apply s1 env
        t' = generalize env' t1
    (s2, t2) <- infer (env' `extend` (TV $ show i, t')) body
    return (s2 `compose` s1, t2)
  A.BindGlobal (A.IdentifierPattern _) e -> infer env e
  A.Block lines ->
    infer env (last lines) -- yeah this won't work lol
  A.FunctionApplication e1 e2 -> do
    tv <- fresh
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    let funcType = case t1 of
          (TFunc _ _) -> TFunc
          TImpureFunc _ _ -> TImpureFunc
          _ -> error "function application on non-function"
    s3 <- unify (apply s2 t1) (funcType t2 tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)
  other -> throwError $ Other $ "Can't infer type of " ++ show other

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1

unify :: Type -> Type -> Infer Subst
unify (l `TFunc` r) (l' `TFunc` r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)
unify (l `TImpureFunc` r) (l' `TImpureFunc` r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)
unify (TVariable a) t = bind a t
unify t (TVariable a) = bind a t
unify (TConcrete a) (TConcrete b) | a == b = return M.empty
unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: TVar -> Type -> Infer Subst
bind a t
  | t == TVariable a = return M.empty
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return $ M.singleton a t

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t
