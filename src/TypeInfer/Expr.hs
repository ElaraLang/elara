module TypeInfer.Expr where

import Control.Monad.Except
import Control.Monad.RWS (listen)
import Control.Monad.State
import Data.Maybe (fromJust, isJust)
import Debug.Trace (traceShowM)
import Interpreter.AST (matchCaseExpression)
import qualified Interpreter.AST as A
import TypeInfer.Env

funcCtor :: Type -> (Type -> Type -> Type)
funcCtor fT t1 tv = case fT of
  TImpure _ -> TImpure (t1 `TFunc` tv)
  _ -> t1 `TFunc` tv

inferExpression :: A.Expression -> Infer Type
inferExpression ex = case ex of
  A.Constant c -> inferConstant c
  A.List [] -> TConApp (TCon "List") <$> fresh
  A.List (x : xs) -> do
    t <- inferExpression x
    ts <- inferExpression (A.List xs)
    let listType = TConApp (TCon "List") t
    uni listType ts
    return listType
  A.Cons x xs -> do
    t <- inferExpression x
    ts <- inferExpression xs
    let listCon = if isImpure t || isImpure ts then TImpure . (TConApp (TCon "List") . purify) else TConApp (TCon "List")
    uni (listCon t) ts
    return $ listCon t
  A.Reference x -> lookupEnv (show x)
  A.Lambda param body rec -> do
    tv <- fresh
    t <- inEnv (show param, Forall [] tv) (inferExpression body)
    -- If recursive, always pure and don't purify the body
    let res = if rec then tv `TFunc` t else funcCtor t tv (purify t)
    return res
  A.BindWithBody name val body -> do
    env <- gets typeEnv
    (t0, constraints) <- listen $ inferExpression val
    subst <- liftEither $ runSolve constraints
    let sc = generalize (apply subst env) (apply subst t0)
    bodyType <- inEnv (show name, sc) $ inferExpression body
    expected <- maybeLookupEnv (show name)
    when (isJust expected) $ do
      let actual = fromJust expected
      uni bodyType actual
    return bodyType
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
    uni t1 (funcCtor t1 t2 tv)
    traceShowM (f, t1)
    let appRes = case t1 of -- If the function is impure, an impure result is returned
          TImpure _ -> TImpure tv
          _ -> tv
    return appRes
  A.Block es -> do
    xs <- mapM inferExpression es
    return $
      if any isImpure xs
        then impurifyOnce (last xs)
        else last xs
  A.Fix e -> do
    t1 <- inferExpression e
    tv <- fresh
    uni (funcCtor t1 tv tv) t1
    return tv
  A.IfElse condition t f -> do
    t1 <- inferExpression condition
    t2 <- inferExpression t
    t3 <- inferExpression f
    uni t1 (TCon "Bool")
    uni t2 t3
    return t2
  A.Match val cases -> do
    t <- inferExpression val
    expected <- fresh
    caseTypes <-
      withCopyOfEnv $
        forM
          cases
          ( \(A.MatchCase pattern body) -> do
              pat <- inferPattern pattern
              uni pat t
              bodyType <- inferExpression body
              uni expected bodyType
              return bodyType
          )
    return $ if any isImpure caseTypes then impurify expected else expected

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
inferPattern A.WildcardPattern = fresh
inferPattern (A.ListPattern xs) = do
  ts <- mapM inferPattern xs
  l <- if null ts then fresh else return $ last ts
  return $ TConApp (TCon "List") l
inferPattern (A.ConsPattern x xs) = do
  t1 <- inferPattern x
  _ <- inferPattern xs
  return $ TConApp (TCon "List") t1
