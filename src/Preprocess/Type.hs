module Preprocess.Type where

import qualified Interpreter.AST as I
import qualified Parse.AST as P

preprocessType :: P.Type -> I.Type
preprocessType (P.NamedT name) = I.NamedType name
preprocessType (P.VarT name) = I.VarType (I.TypeVariable name)
preprocessType (P.ListT t) = I.ListType (preprocessType t)
preprocessType (P.PureFunT a b) = I.PureFunctionType (preprocessType a) (preprocessType b)
preprocessType (P.ImpureFunT a b) = I.ImpureFunctionType (preprocessType a) (preprocessType b)
