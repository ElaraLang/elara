module Elara.TypeInfer.Environment where

import Data.Map (insert)
import Elara.AST.Name (VarName)
import Elara.AST.Typed (PartialType, VarRef)
import Elara.Data.Pretty

newtype TypeEnvironment = TypeEnvironment (Map (VarRef VarName) PartialType) deriving (Show, Pretty)

addToEnv :: VarRef VarName -> PartialType -> TypeEnvironment -> TypeEnvironment
addToEnv varRef partialType (TypeEnvironment env) = TypeEnvironment (insert varRef partialType env)