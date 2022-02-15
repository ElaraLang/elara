module Preprocess.Type where

import qualified Interpreter.AST as I
import qualified Parse.AST as P

preprocessType :: P.Type -> I.Type
preprocessType (P.NamedT name) = I.NamedType name
preprocessType (P.ListT t) = I.ListType (preprocessType t)
preprocessType (P.PureFunT a b) = I.PureFunctionType (preprocessType a) (preprocessType b)
