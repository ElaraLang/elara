module TypeInferrer.Infer where
import TypeInferrer.Env
import qualified Data.Map as M 
import Control.Monad.Except (runExceptT)
import Control.Monad.State.Lazy (evalState)
import TypeInferrer.Type
import qualified Interpreter.AST as A

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = do
  res <- evalState (runExceptT m) (Unique 0)
  return $ closeOver res
  

closeOver :: (M.Map TVar Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
  where
    sc = generalize baseEnv (apply sub ty)
    
inferLine :: TypeEnv -> A.Line -> Infer (Subst, Type)
inferLine env (A.ExpressionLine e) = infer env e

