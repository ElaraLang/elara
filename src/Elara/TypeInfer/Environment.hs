module Elara.TypeInfer.Environment where

import Data.Map (insert)
import Elara.AST.Name (VarName)
import Elara.AST.Typed (PartialType, VarRef)
import Elara.Data.Pretty

newtype TypeEnvironment = TypeEnvironment (Map (VarRef VarName) PartialType) deriving (Show, Pretty)

instance Semigroup TypeEnvironment where
    TypeEnvironment a <> TypeEnvironment b = TypeEnvironment (a <> b)

instance Monoid TypeEnvironment where
    mempty = TypeEnvironment mempty

addToEnv :: VarRef VarName -> PartialType -> TypeEnvironment -> TypeEnvironment
addToEnv varRef partialType (TypeEnvironment env) = TypeEnvironment (insert varRef partialType env)