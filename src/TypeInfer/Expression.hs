module TypeInfer.Expression where

import AST.Canonical qualified as Can
import Control.Monad.Except (liftEither)
import Control.Monad.RWS.Strict (MonadWriter (listen), gets)
import Elara.Name qualified as Name
import Print (debugColored)
import TypeInfer.Env (Infer)
import TypeInfer.Env qualified as E
import TypeInfer.Pattern (inferPattern)
import TypeInfer.Type qualified as T

inferExpression :: Can.Expr -> Infer T.Type
inferExpression ex = case ex of
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
  Can.FunctionCall f x -> do
    t1 <- inferExpression f
    t2 <- inferExpression x
    tv <- E.freshTVar
    E.uni t1 (t2 `T.TFunc` tv)
    return tv
  Can.LetIn def expr body -> do
    let name = Can.defName def
    let pats = Can.defPatterns def

    (functionType, constraints) <- E.withCopyOfEnv $ do
      patternTypes <- mapM inferPattern pats
      (t0, constraints) <- listen $ inferExpression expr
      let func = foldr T.TFunc t0 patternTypes
      debugColored $ show (pats `zip` patternTypes) <> " + " <> show expr <> " :: " <> show t0 <> "= " <> show func
      return (func, constraints)

    subst <- liftEither $ E.runSolve constraints
    env <- gets E.typeEnv

    let sc = E.generalize (E.apply subst env) (E.apply subst functionType)
    E.inEnv (Name.value name, sc) $ inferExpression body
  Can.Let def expr -> do
    let name = Can.defName def
    i <- inferExpression (Can.LetIn def expr (Can.Var name))
    env <- gets E.typeEnv
    let sc = E.generalize env i
    E.addToEnv (Name.value name, sc)
    return (T.TCon "()")
  -- TODO the others
  other -> error $ "Cannot infer expression: " ++ show other