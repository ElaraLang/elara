module Elara.TypeInfer.Environment where

import Data.Map (insert)
import Elara.AST.Name (VarName)
import Elara.AST.Typed (PartialType, VarRef)

newtype TypeEnvironment = TypeEnvironment (Map (VarRef VarName) PartialType) deriving (Show)

addToEnv :: VarRef VarName -> PartialType -> TypeEnvironment -> TypeEnvironment
addToEnv varRef partialType (TypeEnvironment env) = TypeEnvironment (insert varRef partialType env)