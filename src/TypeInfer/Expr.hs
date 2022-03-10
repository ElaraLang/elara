module TypeInfer.Expr where

import Control.Monad.Except
import Control.Monad.RWS (listen)
import Control.Monad.State
import Debug.Trace (traceShowM)
import Interpreter.AST (matchCaseExpression)
import qualified Interpreter.AST as A
import TypeInfer.Env

inferExpression :: A.Expression -> Infer Type
inferExpression ex = case ex of
  A.Constant c -> inferConstant c
  A.List [] -> TConApp (TCon "List") <$> fresh
  A.List (x : xs) -> do
    t <- inferExpression x
    ts <- inferExpression (A.List xs)
    uni t ts
    return $ TConApp (TCon "List") t
  A.Cons x xs -> do
    t <- inferExpression x
    ts <- inferExpression xs
    uni (TConApp (TCon "List") t) ts
    return $ TConApp (TCon "List") t
  A.Reference x -> lookupEnv (show x)
  A.Lambda param body -> do
    tv <- fresh
    t <- inEnv (show param, Forall [] tv) (inferExpression body)
    return (tv `TFunc` t)
  A.BindWithBody name val body -> do
    env <- gets typeEnv
    (t0, constraints) <- listen $ inferExpression val
    subst <- liftEither $ runSolve constraints
    let sc = generalize (apply subst env) (apply subst t0)
    inEnv (show name, sc) $ inferExpression body
  A.BindGlobal name val -> do
    i <- inferExpression (A.BindWithBody name val (A.Reference name))
    env <- gets typeEnv
    let sc = generalize env i
    addToEnv (show name, sc)
    return (TCon "()")
  A.FunctionApplication f arg -> do
    t1 <- inferExpression f
    t2 <- inferExpression arg
    tv <- fresh
    uni t1 (t2 `TFunc` tv)
    return tv
  A.Block es -> do
    xs <- mapM inferExpression es
    return $ last xs
  A.Fix e -> do
    t1 <- inferExpression e
    tv <- fresh
    uni (tv `TFunc` tv) t1
    return tv
  A.IfElse cond t f -> do
    t1 <- inferExpression cond
    t2 <- inferExpression t
    t3 <- inferExpression f
    uni t1 (TCon "Bool")
    uni t2 t3
    return t2
  A.Match val (main : others) -> do
    t <- inferExpression val
    expected <- inferExpression . matchCaseExpression $ main
    withCopyOfEnv $
      forM_
        others
        ( \(A.MatchCase pattern body) -> do
            pat <- inferPattern pattern
            uni pat t
            inferExpression body >>= uni expected
        )
    return expected
  other -> throwError $ Other $ "Cannot infer type of expression: " ++ show other

inferConstant :: A.Constant -> Infer Type
inferConstant (A.IntC _) = return $ TCon "Int"
inferConstant A.UnitC = return $ TCon "()"
inferConstant (A.StringC _) = return $ TCon "String"

inferPattern :: A.Pattern -> Infer Type
inferPattern (A.ConstantPattern c) = inferConstant c
inferPattern (A.IdentifierPattern x) = do
  ty <- fresh
  addToEnv (show x, Forall [] ty)
  return ty
inferPattern (A.WildcardPattern) = fresh
inferPattern (A.ListPattern xs) = do
  ts <- mapM inferPattern xs
  l <- if null ts then fresh else return $ last ts
  return $ TConApp (TCon "List") l
inferPattern (A.ConsPattern x xs) = do
  t1 <- inferPattern x
  _ <- inferPattern xs
  return $ TConApp (TCon "List") t1
