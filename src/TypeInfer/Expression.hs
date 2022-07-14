module TypeInfer.Expression where

import AST.Canonical qualified as Can
import Control.Monad (when)
import Control.Monad.Except (liftEither)
import Control.Monad.RWS.Strict (MonadWriter (listen), gets)
import Data.Maybe (fromJust, isJust)
import Data.Text qualified as Text
import Elara.Name qualified as Name
import Print (debugColored, debugColoredStr)
import TypeInfer.Env (Infer)
import TypeInfer.Env qualified as E
import TypeInfer.Pattern (inferPattern)
import TypeInfer.Type qualified as T

inferExpression :: Can.Expr -> Infer T.Type
inferExpression expr = do
  res <- inferExpression' expr
  debugColoredStr $ "Infer " <> show expr <> " :: " <> show res
  return res

inferExpression' :: Can.Expr -> Infer T.Type
inferExpression' ex = case ex of
  Can.Char _ -> return $ T.TCon "Char"
  Can.Int _ -> return $ T.TCon "Int"
  Can.String _ -> return $ T.TCon "String"
  Can.Float _ -> return $ T.TCon "Float"
  Can.Var name -> E.lookupEnv (Name.value name)
  Can.List [] -> T.TApp (T.TCon "[]") <$> E.freshTVar
  Can.List (x : xs) -> do
    t <- inferExpression x
    ts <- inferExpression (Can.List xs)
    let listType = T.TApp (T.TCon "[]") t
    E.uni listType ts
    return listType
  Can.BlockExpr exprs -> do
    let (initExprs, lastExpr) = (init exprs, last exprs)
    mapM_ inferExpression' initExprs
    inferExpression lastExpr
  Can.Lambda pats body -> do
    (patternTypes, b) <- E.withCopyOfEnv $ do
      patternTypes <- mapM inferPattern pats
      b <- inferExpression body
      return (patternTypes, b)
    return $ foldr T.TFunc b patternTypes
  Can.FunctionCall f arg -> do
    t1 <- inferExpression f
    t2 <- inferExpression arg
    res <- E.freshTVar
    E.uni t1 (t2 `T.TFunc` res) -- type of f == type of arg -> res
    return res
  Can.BinOp op l r -> do
    inferExpression (Can.FunctionCall (Can.FunctionCall op l) r) -- TODO make a way of doing this that will accurately report errors
  Can.LetIn def expr body -> letInfer def expr (\(name, _, sc) -> E.inEnv (name, sc) $ inferExpression body)
  Can.Let def expr -> letInfer def expr (\(_, t, _) -> return t)
  -- TODO the others
  other -> error $ "Cannot infer expression: " ++ show other

letInfer :: Can.Def -> Can.Expr -> ((Text.Text, T.Type, T.Scheme) -> Infer T.Type) -> Infer T.Type
letInfer def expr func = do
  let name = Can.defName def
  let pats = Can.defPatterns def
  ((functionTypes, t0), constraints) <- listen $
    E.withCopyOfEnv $ do
      functionTypes <- mapM inferPattern pats
      val <- inferExpression expr
      return (functionTypes, val)

  let functionType = foldr T.TFunc t0 functionTypes
  subst <- liftEither $ E.runSolve constraints
  env <- gets E.typeEnv
  let sc = E.generalize (E.apply subst env) (E.apply subst functionType)
  bodyType <- func (Name.value name, functionType, sc)
  expected <- E.maybeLookupEnv (Name.value name)
  when (isJust expected) $ do
    let actual = fromJust expected
    E.uni bodyType actual
  E.addToEnv (Name.value name, sc)
  return bodyType

