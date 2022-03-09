module TypeInferrer.Infer where

import Control.Monad (foldM)
import Control.Monad.Except (runExcept)
import Control.Monad.RWS (runRWST)
import qualified Interpreter.AST as A
import TypeInferrer.Env
import TypeInferrer.Expr

runInfer :: TypeEnv -> Infer a -> Either TypeError (a, TypeEnv, [Constraint])
runInfer env m = runExcept $ (\(res, state, constraints) -> (res, typeEnv state, constraints)) <$> runRWST m () (InferState 0 env)

infer :: Infer Type -> TypeEnv -> Either TypeError (TypeEnv, Scheme)
infer inf env = do
  (scheme, nextEnv, cs) <- runInfer env inf
  subst <- runSolve cs
  return (nextEnv, closeOver $ apply subst scheme)

inferExpr :: A.Expression -> TypeEnv -> Either TypeError (TypeEnv, Scheme)
inferExpr e = infer (inferExpression e)

inferLine :: A.Line -> TypeEnv -> Either TypeError (TypeEnv, Scheme)
inferLine (A.ExpressionLine e) = inferExpr e

inferLines :: [A.Line] -> TypeEnv -> Either TypeError [Scheme]
inferLines lines startEnv = do
  -- Infer all lines, threading the type environment through
  (_, schemes) <-
    foldM
      ( \(env, acc) line -> do
          (env', scheme) <- inferLine line env
          return (env', scheme : acc)
      )
      (startEnv, [])
      lines
  return $ reverse schemes

inferTypeDef :: TypeEnv -> A.TypeDefinition -> Infer (Subst, Scheme)
inferTypeDef env (A.TypeDefinition name args ty) = do
  let typeVariables = TV . show <$> args
  error "help"
