module TypeInfer.ASTType where

import AST.Canonical qualified as Can
import Elara.Name qualified as Name
import TypeInfer.Env (Infer)
import TypeInfer.Env qualified as E
import TypeInfer.Type qualified as T

inferType :: Can.Type -> Infer T.Type
inferType (Can.TVar name) = return $ T.TVariable $ T.TV $ Name.value name
inferType (Can.TUnit) = return $ T.TCon "()"
inferType (Can.TLambda a b) = do
  ta <- inferType a
  tb <- inferType b
  return $ T.TFunc ta tb
inferType (Can.TConstructorApp con ty) = do
  con' <- inferType con
  ty' <- inferType ty
  return $ T.TApp con' ty'
