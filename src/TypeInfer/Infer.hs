module TypeInfer.Infer where

import Control.Monad (foldM)
import Control.Monad.Except (runExcept)
import Control.Monad.RWS (runRWST)
import Debug.Trace (traceShowM)
import qualified Interpreter.AST as A
import TypeInfer.Env
import TypeInfer.Expr as E
import TypeInfer.Type

runInfer :: TypeEnv -> Infer a -> Either TypeError (a, TypeEnv, [Constraint])
runInfer env (Infer m) = runExcept $ (\(res, state, constraints) -> (res, typeEnv state, constraints)) <$> runRWST m () (InferState 0 env)

infer :: Infer Type -> TypeEnv -> Either TypeError (TypeEnv, Scheme)
infer inf env = do
  (scheme, nextEnv, cs) <- runInfer env inf
  subst <- runSolve cs
  return (nextEnv, closeOver $ apply subst scheme)

inferExpr :: A.Expression -> TypeEnv -> Either TypeError (TypeEnv, Scheme)
inferExpr e = infer (E.inferExpression e)

inferDefLine :: A.Identifier -> A.Type -> TypeEnv -> Either TypeError (TypeEnv, Scheme)
inferDefLine name t env = do
  (env', scheme) <- infer (inferType t) env
  (_, env'', _) <- runInfer env' (addToEnv (show name, scheme))
  return (env'', scheme)

inferLine :: A.Line -> TypeEnv -> Either TypeError (TypeEnv, Scheme)
inferLine (A.ExpressionLine e) = inferExpr e
inferLine (A.DefLine name ty) = inferDefLine name ty
inferLine other = error $ "inferLine: " ++ show other

inferLines :: [A.Line] -> TypeEnv -> Either TypeError TypeEnv
inferLines l startEnv = do
  -- Infer all lines, threading the type environment through
  (env, _) <-
    foldM
      ( \(env, acc) line -> do
          (env', scheme) <- inferLine line env
          return (env', scheme : acc)
      )
      (startEnv, [])
      l
  return env

inferTypeDef :: TypeEnv -> A.TypeDefinition -> Infer (Subst, Scheme)
inferTypeDef _ (A.TypeDefinition _ args _) = do
  let _ = TV . show <$> args
  error "help"
