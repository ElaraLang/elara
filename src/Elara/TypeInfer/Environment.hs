module Elara.TypeInfer.Environment where

import Data.Map (insert, lookup)
import Elara.AST.Name (VarName)
import Elara.AST.Typed (PartialType, VarRef, UnlocatedVarRef)
import Elara.Data.Pretty

newtype TypeEnvironment = TypeEnvironment (Map (UnlocatedVarRef VarName) PartialType) deriving (Show, Pretty)

instance Semigroup TypeEnvironment where
    TypeEnvironment a <> TypeEnvironment b = TypeEnvironment (a <> b)

instance Monoid TypeEnvironment where
    mempty = TypeEnvironment mempty

addToEnv :: UnlocatedVarRef VarName -> PartialType -> TypeEnvironment -> TypeEnvironment
addToEnv varRef partialType (TypeEnvironment env) = TypeEnvironment (insert varRef partialType env)

lookupInEnv :: UnlocatedVarRef VarName -> TypeEnvironment -> Maybe PartialType
lookupInEnv varRef (TypeEnvironment env) = lookup varRef env