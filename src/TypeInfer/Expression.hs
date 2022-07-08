module TypeInfer.Expression where

import AST.Canonical qualified as Can
import Control.Monad (when)
import Control.Monad.Except (liftEither)
import Control.Monad.RWS.Strict (MonadWriter (listen), gets)
import Data.Maybe (fromJust, isJust)
import Elara.Name qualified as Name
import Print (debugColored, debugColoredStr)
import TypeInfer.Env (Infer)
import TypeInfer.Env qualified as E
import TypeInfer.Infer (infer, runInfer)
import TypeInfer.Pattern (inferPattern)
import TypeInfer.Type qualified as T

inferExpression :: Can.Expr -> Infer T.Type
inferExpression ex = do
  res <- inferExpression' ex
  -- debugColoredStr $! "Inferring expression: " ++ show ex ++ " :: " ++ show res
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
  Can.Lambda pats body -> do
    (patternTypes, b) <- E.withCopyOfEnv $ do
      patternTypes <- mapM inferPattern pats
      b <- inferExpression body
      return (patternTypes, b)
    return $ foldr T.TFunc b patternTypes
  Can.FunctionCall f x -> do
    t1 <- inferExpression f
    t2 <- inferExpression x
    tv <- E.freshTVar
    E.uni t1 (t2 `T.TFunc` tv)
    return tv
  Can.BinOp op l r -> do
    inferExpression (Can.FunctionCall (Can.FunctionCall op l) r) -- TODO make a way of doing this that will accurately report errors
  Can.LetIn def expr body -> do
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
    bodyType <- E.inEnv (Name.value name, sc) $ inferExpression body
    expected <- E.maybeLookupEnv (Name.value name)
    when (isJust expected) $ do
      let actual = fromJust expected
      E.uni bodyType actual
    return bodyType
  Can.Let def expr -> do
    let name = Can.defName def
    expectedType <- E.maybeLookupEnv (Name.value name)
    i <- inferExpression (Can.LetIn def expr (Can.Var name))
    env <- gets E.typeEnv
    let sc = E.generalize env i
    E.addToEnv (Name.value name, sc)
    when (isJust expectedType) $ do
      let actual = fromJust expectedType
      E.uni i actual
    return (T.TCon "()")
  -- TODO the others
  other -> error $ "Cannot infer expression: " ++ show other
