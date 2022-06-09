module TypeInfer.Value where

import AST.Canonical qualified as Can
import Elara.Name (Name)
import TypeInfer.Env (Infer)
import TypeInfer.Expression (inferExpression)
import TypeInfer.Type qualified as T

inferDef :: Can.Def -> Infer T.Type
inferDef (Can.Def name pat val) = inferDefWithExpectedType name pat val Nothing
inferDef (Can.TypedDef name pat val ty) = inferDefWithExpectedType name pat val (Just ty)


inferDefWithExpectedType :: Name -> [Can.Pattern] -> Can.Expr -> Maybe Can.Type -> Infer T.Type
inferDefWithExpectedType name pats expr expectedType = do
  -- Convert it into a let expression, infer, then add to the global env
  let def = case expectedType of
        Nothing -> Can.Def name pats expr
        Just ty -> Can.TypedDef name pats expr ty
  inferExpression $ Can.Let def expr