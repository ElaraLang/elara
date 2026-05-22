module Elara.TypeInfer.Environment where

import Data.GADT.Compare (GCompare (gcompare), GEq (..), GOrdering (..))
import Data.Map qualified as Map
import Data.Type.Equality
import Effectful
import Effectful.Error.Static
import Effectful.State.Extra (locally)
import Effectful.State.Static.Local
import Elara.AST.Name
import Elara.AST.Region (SourceRegion)
import Elara.Data.Pretty
import Elara.Data.Unique
import Elara.Error
import Elara.Error.Diagnose (toDiagnoseReports)
import Elara.TypeInfer.Type
import Error.Diagnose hiding (Hint, Note)
import Unsafe.Coerce (unsafeCoerce)

-- | A type environment Γ, which maps type variables and data constructors to types
newtype TypeEnvironment loc
    = TypeEnvironment
        (Map (TypeEnvKey loc) (Type loc))
    deriving (Show)

instance Pretty loc => Pretty (TypeEnvironment loc) where
    pretty (TypeEnvironment env) = listToText (pretty <$> Map.toList env)

emptyTypeEnvironment :: TypeEnvironment loc
emptyTypeEnvironment = TypeEnvironment Map.empty

-- | A key in the type environment
data TypeEnvKey loc
    = -- | A data constructor K
      DataConKey DataCon
    | -- | A term variable x
      TermVarKey (Qualified VarName)
    deriving (Show, Eq, Ord, Generic)

-- | This is safe I think because phantom type
instance GEq TypeEnvKey where
    geq :: forall k (a :: k) (b :: k). TypeEnvKey a -> TypeEnvKey b -> Maybe (a :~: b)
    geq (DataConKey a) (DataConKey b) =
        if a == b
            then Just (unsafeCoerce Refl)
            else Nothing
    geq (TermVarKey a) (TermVarKey b) =
        if a == b
            then Just (unsafeCoerce Refl)
            else Nothing
    geq _ _ = Nothing

instance GCompare TypeEnvKey where
    gcompare :: forall k (a :: k) (b :: k). TypeEnvKey a -> TypeEnvKey b -> GOrdering a b
    gcompare (DataConKey a) (DataConKey b) =
        case compare a b of
            LT -> GLT
            EQ -> unsafeCoerce GEQ
            GT -> GGT
    gcompare (DataConKey _) (TermVarKey _) = GLT
    gcompare (TermVarKey _) (DataConKey _) = GGT
    gcompare (TermVarKey a) (TermVarKey b) =
        case compare a b of
            LT -> GLT
            EQ -> unsafeCoerce GEQ
            GT -> GGT

instance Hashable (TypeEnvKey loc)

instance Pretty (TypeEnvKey loc) where
    pretty (DataConKey con) = pretty con
    pretty (TermVarKey name) = pretty name

addType :: TypeEnvKey loc -> Type loc -> TypeEnvironment loc -> TypeEnvironment loc
addType key ty (TypeEnvironment env) = TypeEnvironment (Map.insert key ty env)

addType' :: State (TypeEnvironment loc) :> r => TypeEnvKey loc -> Type loc -> Eff r ()
addType' key ty = modify (addType key ty)

lookupTypeMaybe :: State (TypeEnvironment loc) :> r => TypeEnvKey loc -> Eff r (Maybe (Type loc))
lookupTypeMaybe key = do
    TypeEnvironment env <- get
    pure (Map.lookup key env)

lookupType ::
    ( Error (InferError loc) :> r
    , State (TypeEnvironment loc) :> r
    , Show loc
    ) =>
    TypeEnvKey loc -> Eff r (Type loc)
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

lookupLocalVarType :: (Error (InferError loc) :> r, Show loc) => Unique VarName -> LocalTypeEnvironment loc -> Eff r (Type loc)
lookupLocalVarType var (LocalTypeEnvironment env) =
    case Map.lookup var env of
        Just ty -> pure ty
        Nothing -> throwError (UnboundLocalVar var (LocalTypeEnvironment env))

lookupLocalVar ::
    ( State (LocalTypeEnvironment loc) :> r
    , Error (InferError loc) :> r
    , Show loc
    ) =>
    Unique VarName ->
    Eff r (Type loc)
lookupLocalVar name = get >>= lookupLocalVarType name

-- | An error that can occur during type inference
data InferError loc
    = UnboundTermVar (TypeEnvKey loc) (TypeEnvironment loc)
    | UnboundLocalVar (Unique VarName) (LocalTypeEnvironment loc)
    deriving (Show, Generic)

instance Pretty loc => Pretty (InferError loc)

instance (Show loc, Pretty loc, Typeable loc) => Exception (InferError loc)

instance {-# OVERLAPPABLE #-} (Show loc, Pretty loc, Typeable loc) => ElaraDiagnostic (InferError loc) where
    diagnosticReports _ = []

instance ElaraDiagnostic (InferError SourceRegion) where
    diagnosticMessage (UnboundTermVar (TermVarKey key) _) = "Unbound term variable" <+> pretty key
    diagnosticMessage (UnboundTermVar (DataConKey key) _) = "Unbound data constructor" <+> pretty key
    diagnosticMessage (UnboundLocalVar name _) = "Unbound local variable" <+> pretty name

    diagnosticNotes (UnboundTermVar _ (TypeEnvironment env)) = [Elara.Error.Note $ "Possible names:" <+> listToText (pretty <$> Map.keys env)]
    diagnosticNotes (UnboundLocalVar _ (LocalTypeEnvironment env)) = [Elara.Error.Note $ "Possible names:" <+> listToText (pretty <$> Map.keys env)]
    diagnosticNotes _ = []
