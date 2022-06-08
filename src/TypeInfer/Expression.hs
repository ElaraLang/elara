module TypeInfer.Expression where

import AST.Canonical qualified as Can
import Elara.Name qualified as Name
import TypeInfer.Env (Infer)
import TypeInfer.Env qualified as T

inferExpression :: Can.Expr -> Infer T.Type
inferExpression ex = case ex of
  Can.Char _ -> return $ T.TCon "Char"
  Can.Int _ -> return $ T.TCon "Int"
  Can.String _ -> return $ T.TCon "String"
  Can.Float _ -> return $ T.TCon "Float"
  Can.Var name -> T.lookupEnv (Name.value name)
  Can.List [] -> T.TApp (T.TCon "[]") <$> T.freshTVar
  Can.List (x : xs) -> do
    t <- inferExpression x
    ts <- inferExpression (Can.List xs)
    let listType = T.TApp (T.TCon "[]") t
    T.uni listType ts
    return listType
  -- TODO the others