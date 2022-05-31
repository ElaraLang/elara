module TypeInfer.Type where

import qualified Interpreter.AST as A
import TypeInfer.Env

inferType :: A.Type -> Infer Type
inferType (A.VarType v) = return $ TVariable $ TV $ show v
inferType (A.NamedType n) = return $ TCon n
inferType (A.ListType n) = do
  t <- inferType n
  return $ TConApp (TCon "List") t
inferType A.UnitType = return $ TCon "()"
inferType (A.PureFunctionType a b) = do
  ta <- inferType a
  tb <- inferType b
  return $ TFunc ta tb
inferType (A.ImpureFunctionType a b) = do
  ta <- inferType a
  tb <- inferType b
  return $ TImpure (TFunc ta tb)
inferType (A.ConstructorAppType a b) = do
  ta <- inferType a
  tb <- inferType b
  return $ TConApp ta tb