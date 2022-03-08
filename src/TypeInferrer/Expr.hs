module TypeInferrer.Expr where

import Control.Monad.Except
import Control.Monad.RWS (ask, listen)
import Control.Monad.State
import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Interpreter.AST as A
import TypeInferrer.Env

inferExpression :: A.Expression -> Infer Type
inferExpression ex = case ex of
  A.Constant (A.IntC _) -> return $ TConcrete "Int"
  A.Reference x -> lookupEnv (show x)
  A.Lambda param body -> do
    tv <- fresh
    t <- inEnv (show param, Forall [] tv) (inferExpression body)
    return (tv `TFunc` t)
  A.BindWithBody name val body -> do
    env <- gets typeEnv
    t1 <- inferExpression val
    let sc = generalize env t1
    inEnv (show name, sc) (inferExpression body)
  A.BindGlobal name val -> do
    env <- gets typeEnv
    t1 <- inferExpression val
    let sc = generalize env t1
    addToEnv (show name, sc)
    return (TConcrete "()")
  A.FunctionApplication f arg -> do
    t1 <- inferExpression f
    t2 <- inferExpression arg
    tv <- fresh
    uni t1 (t2 `TFunc` tv)
    return tv
  other -> throwError $ Other $ "Cannot infer type of expression: " ++ show other
