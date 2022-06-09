module TypeInfer.Value where

import AST.Canonical qualified as Can
import Control.Monad.RWS (MonadReader (ask), gets, forM)
import Elara.Name (Name)
import Elara.Name qualified as Name
import TypeInfer.Env (Infer, Type, uni)
import TypeInfer.Env qualified as T
import TypeInfer.Expression (inferExpression)
import TypeInfer.Type (inferType)
inferDef :: Can.Def -> Infer Type
inferDef (Can.Def name pat val) = T.freshTVar >>= inferDefWithExpectedType name pat val
inferDef (Can.TypedDef name pat val ty) = inferType ty >>= inferDefWithExpectedType name pat val

inferDefWithExpectedType :: Name -> [Can.Pattern] -> Can.Expr -> Type -> Infer Type
inferDefWithExpectedType name pats expr t = do
  forM pats $ \(Can.PVar name) -> do -- TODO bring this into a separate function for the other pattern types
    tv <- T.freshTVar
    env <- gets T.typeEnv
    let sc = T.generalize env tv
    T.addToEnv (Name.value name, sc)

  inferred <- inferExpression expr
  env <- gets T.typeEnv
  let sc = T.generalize env inferred
  T.addToEnv (Name.value name, sc)
  uni inferred t
  return inferred
