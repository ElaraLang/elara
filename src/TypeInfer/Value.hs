module TypeInfer.Value where

import AST.Canonical qualified as Can
import Control.Monad.RWS (gets)
import Elara.Name (Name)
import Elara.Name qualified as Name
import TypeInfer.ASTType (inferType)
import TypeInfer.Env (Infer)
import TypeInfer.Env qualified as Env
import TypeInfer.Expression (inferExpression)
import TypeInfer.Type qualified as T

inferDef :: Can.Def -> Infer T.Type
inferDef def@(Can.Def _ _ val) = inferExpression $ Can.Let def val -- Convert it into a let expression, infer, then add to the global env
inferDef (Can.TypedDef name ty) = do
  type' <- inferType ty
  env <- gets Env.typeEnv
  let scheme = Env.generalize env type'
  Env.addToEnv (Name.value name, scheme)
  return type'
