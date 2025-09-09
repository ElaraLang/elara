module Elara.TypeInfer.Environment where

import Data.GADT.Compare (GEq (..))
import Data.GADT.Compare.TH (deriveGEq)
import Data.Kind qualified as Kind
import Data.Map qualified as Map
import Data.Type.Equality
import Effectful
import Effectful.Error.Static
import Effectful.State.Extra (locally)
import Effectful.State.Static.Local
import Elara.AST.Name
import Elara.Data.Pretty
import Elara.Data.Unique
import Elara.Error
import Elara.TypeInfer.Type
import Error.Diagnose

-- | A type environment Γ, which maps type variables and data constructors to types
newtype TypeEnvironment loc
    = TypeEnvironment
        (Map TypeEnvKey (Type loc))
    deriving (Show)

instance Pretty loc => Pretty (TypeEnvironment loc) where
    pretty (TypeEnvironment env) = listToText (pretty <$> Map.toList env)

emptyTypeEnvironment :: TypeEnvironment loc
emptyTypeEnvironment = TypeEnvironment Map.empty

-- | A key in the type environment
data TypeEnvKey
    = -- | A data constructor K
      DataConKey DataCon
    | -- | A term variable x
      TermVarKey (Qualified VarName)
    deriving (Show, Eq, Ord, Generic)

instance Hashable TypeEnvKey

instance Pretty TypeEnvKey where
    pretty (DataConKey con) = pretty con
    pretty (TermVarKey name) = pretty name

addType :: TypeEnvKey -> Type loc -> TypeEnvironment loc -> TypeEnvironment loc
addType key ty (TypeEnvironment env) = TypeEnvironment (Map.insert key ty env)

addType' :: State (TypeEnvironment loc) :> r => TypeEnvKey -> Type loc -> Eff r ()
addType' key ty = modify (addType key ty)

lookupTypeMaybe :: State (TypeEnvironment loc) :> r => TypeEnvKey -> Eff r (Maybe (Type loc))
lookupTypeMaybe key = do
    TypeEnvironment env <- get
    pure (Map.lookup key env)

lookupType ::
    ( Error (InferError loc) :> r
    , State (TypeEnvironment loc) :> r
    ) =>
    TypeEnvKey -> Eff r (Type loc)
lookupType key = do
    env'@(TypeEnvironment env) <- get
    case Map.lookup key env of
        Just ty -> pure ty
        Nothing -> throwError (UnboundTermVar key env')

newtype LocalTypeEnvironment loc
    = LocalTypeEnvironment
        (Map (Unique VarName) (Type loc))
    deriving (Show)
    deriving newtype (Pretty)

emptyLocalTypeEnvironment :: LocalTypeEnvironment loc
emptyLocalTypeEnvironment = LocalTypeEnvironment Map.empty

addLocalType :: Unique VarName -> Type loc -> LocalTypeEnvironment loc -> LocalTypeEnvironment loc
addLocalType var ty (LocalTypeEnvironment env) = LocalTypeEnvironment (Map.insert var ty env)

withLocalType :: State (LocalTypeEnvironment loc) :> r => Unique VarName -> Type loc -> Eff r a -> Eff r a
withLocalType var ty = locally (addLocalType var ty)

lookupLocalVarType :: Error (InferError loc) :> r => Unique VarName -> LocalTypeEnvironment loc -> Eff r (Type loc)
lookupLocalVarType var (LocalTypeEnvironment env) =
    case Map.lookup var env of
        Just ty -> pure ty
        Nothing -> throwError (UnboundLocalVar var (LocalTypeEnvironment env))

lookupLocalVar ::
    ( State (LocalTypeEnvironment loc) :> r
    , Error (InferError loc) :> r
    ) =>
    Unique VarName ->
    Eff r (Type loc)
lookupLocalVar name = get >>= lookupLocalVarType name

-- | An error that can occur during type inference
data InferError loc
    = UnboundTermVar TypeEnvKey (TypeEnvironment loc)
    | UnboundLocalVar (Unique VarName) (LocalTypeEnvironment loc)
    deriving (Show, Generic)

instance Pretty loc => Pretty (InferError loc)

instance Pretty loc => ReportableError (InferError loc) where
    report (UnboundTermVar (TermVarKey key) (TypeEnvironment env)) =
        writeReport $
            Err
                Nothing
                ("Unbound term variable " <> pretty key)
                []
                [Note $ "Possible names:" <> listToText (pretty <$> Map.keys env)]
    report o = defaultReport o
