module Elara.TypeInfer.Environment where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Map qualified as Map
import Elara.AST.Name
import Elara.AST.VarRef
import Elara.Data.Unique
import Elara.Error
import Elara.TypeInfer.Type
import Error.Diagnose
import Polysemy (Member, Sem)
import Polysemy.Error
import Polysemy.State
import Polysemy.State.Extra

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
      TermVarKey (Qualified VarName)
    deriving (Show, Eq, Ord)

addType :: TypeEnvKey loc -> Type loc -> TypeEnvironment loc -> TypeEnvironment loc
addType key ty (TypeEnvironment env) = TypeEnvironment (Map.insert key ty env)

addType' key ty = modify (addType key ty)

lookupType ::
    ( Member (Error (InferError loc)) r
    , Member (State (TypeEnvironment loc)) r
    ) =>
    TypeEnvKey loc -> Sem r (Type loc)
lookupType key = do
    env'@(TypeEnvironment env) <- get
    case Map.lookup key env of
        Just ty -> pure ty
        Nothing -> throw (UnboundTermVar key env')

data LocalTypeEnvironment loc
    = LocalTypeEnvironment
        (Map (Unique VarName) (Type loc))
    deriving (Show)

emptyLocalTypeEnvironment :: LocalTypeEnvironment loc
emptyLocalTypeEnvironment = LocalTypeEnvironment Map.empty

addLocalType :: Unique VarName -> Type loc -> LocalTypeEnvironment loc -> LocalTypeEnvironment loc
addLocalType var ty (LocalTypeEnvironment env) = LocalTypeEnvironment (Map.insert var ty env)

withLocalType :: Member (State (LocalTypeEnvironment loc)) r => Unique VarName -> Type loc -> Sem r a -> Sem r a
withLocalType var ty = locally (addLocalType var ty)

lookupLocalVarType :: Member (Error (InferError loc)) r => Unique VarName -> LocalTypeEnvironment loc -> Sem r (Type loc)
lookupLocalVarType var (LocalTypeEnvironment env) =
    case Map.lookup var env of
        Just ty -> pure ty
        Nothing -> throw (UnboundLocalVar var (LocalTypeEnvironment env))

lookupLocalVar ::
    ( Member (State (LocalTypeEnvironment loc)) r
    , Member (Error (InferError loc)) r
    ) =>
    Unique VarName ->
    Sem r (Type loc)
lookupLocalVar name = get >>= lookupLocalVarType name

-- | An error that can occur during type inference
data InferError loc
    = UnboundTermVar (TypeEnvKey loc) (TypeEnvironment loc)
    | UnboundLocalVar (Unique VarName) (LocalTypeEnvironment loc)
    deriving (Show)

instance ReportableError (InferError loc) where
    report (UnboundTermVar key env) =
        writeReport $ Err (Nothing) ("Unbound term variable") [] []
    report (UnboundLocalVar var env) =
        writeReport $ Err (Nothing) ("Unbound local variable") [] []
