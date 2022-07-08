module TypeInfer.Infer where

import Control.Monad (foldM)
import Control.Monad.Except (runExcept)
import Control.Monad.RWS.Strict (runRWST)
import Data.Bifunctor
import Print (debugColored)
import TypeInfer.Env (Constraint, Infer (Infer), InferState (..), Substitutable (apply), TypeEnv, TypeError, closeOver, emptyInferState, runSolve)
import TypeInfer.Type

runInfer :: TypeEnv -> Infer a -> Either TypeError (a, TypeEnv, [Constraint])
runInfer env (Infer m) =
  let res = runRWST m () (emptyInferState env)
   in runExcept $ first typeEnv <$> res

infer :: Infer Type -> TypeEnv -> Either TypeError (TypeEnv, Scheme)
infer inf env = do
  (scheme, nextEnv, cs) <- runInfer env inf
  subst <- runSolve cs
  return (nextEnv, closeOver $ apply subst scheme)

inferMany :: Infer [Type] -> TypeEnv -> Either TypeError (TypeEnv, [Scheme])
inferMany inf env = do
  (schemes, nextEnv, cs) <- runInfer env inf
  subst <- runSolve cs
  return (nextEnv, closeOver . apply subst <$> schemes)