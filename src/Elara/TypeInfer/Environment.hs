module Elara.TypeInfer.Environment where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Map qualified as Map
import Elara.AST.Name
import Elara.AST.VarRef
import Elara.TypeInfer.Type
import Polysemy (Member, Sem)
import Polysemy.Error

-- | A type environment Γ, which maps type variables and data constructors to types
newtype TypeEnvironment loc
    = TypeEnvironment
        (Map (TypeEnvKey loc) (Type loc))
    deriving (Show)

emptyTypeEnvironment :: TypeEnvironment loc
emptyTypeEnvironment = TypeEnvironment Map.empty

-- | A key in the type environment
data TypeEnvKey loc
    = -- | A data constructor K
      DataConKey DataCon
    | -- | A term variable x
      TermVarKey (UnlocatedVarRef VarName)
    deriving (Show, Eq, Ord)

addType :: TypeEnvKey loc -> Type loc -> TypeEnvironment loc -> TypeEnvironment loc
addType key ty (TypeEnvironment env) = TypeEnvironment (Map.insert key ty env)

lookupType :: Member (Error (InferError loc)) r => TypeEnvKey loc -> TypeEnvironment loc -> Sem r (Type loc)
lookupType key env'@(TypeEnvironment env) =
    case Map.lookup key env of
        Just ty -> pure ty
        Nothing -> throw (UnboundTermVar key env')

-- | An error that can occur during type inference
data InferError loc
    = UnboundTermVar (TypeEnvKey loc) (TypeEnvironment loc)
    deriving (Show)
