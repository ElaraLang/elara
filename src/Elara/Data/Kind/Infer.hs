{-# LANGUAGE RecordWildCards #-}

{- | Performs kind inference on a type.
Algorithm inspired by https://gilmi.me/blog/post/2023/09/30/kind-inference

>> infer "type alias Identity a = a"
"k -> k"

>> infer "type Maybe a = Either a Unit"
Type -> Type

>> infer "type Either a b = Left a | Right b"
Type -> Type -> Type

>> infer "type Proxy a = Proxy"
k -> Type

TODO: this is extremely basic and needs some nicer error messages, no support for polykinds or higher-kinded types yet
-}
module Elara.Data.Kind.Infer (
    KindInferError (..),
    inferKind,
    inferTypeKind,
    initialInferState,
    lookupKindVarMaybe,
    lookupNameKindVar,
    preRegisterType,
    InferState,
)
where

import Control.Monad (foldM)
import Data.Generics.Product (field)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Elara.AST.Name (LowerAlphaName, Qualified, TypeName)
import Elara.AST.Phase
import Elara.AST.Phases.Kinded qualified as NewK
import Elara.AST.Phases.MidKinded qualified as NewM
import Elara.AST.Phases.Shunted qualified as NewS
import Elara.AST.Region (Located, SourceRegion, sourceRegionToDiagnosePosition, unlocated)
import Elara.AST.Types qualified as New
import Elara.Data.Kind
import Elara.Data.Pretty
import Elara.Data.Unique (Unique)
import Elara.Data.Unique.Effect
import Elara.Error
import Elara.Logging (debugWith, logDebug)
import Elara.Prim (primKindCheckContext)
import Elara.Query qualified
import Elara.Query.Effects (QueryEffects)
import Error.Diagnose
import Optics (set, traverseOf_)
import Rock qualified

{- | Tracks the origin of a kind constraint for better error messages.
This represents the "why" of why a constraint was added.
-}
data ConstraintOrigin
    = -- | A (type-level) function was applied to an argument
      FunctionApplication
        { functionType :: NewS.ShuntedType
        -- ^ The function being applied
        , functionArg :: (NewS.ShuntedType, NewM.MidKindedType)
        -- ^ The argument type
        }
    | -- | The result of a function application must be of kind Type
      FunctionResult
    | -- | Elements of a list must be of kind Type
      ListElement
    | -- | Field of a record must be of kind Type
      RecordField LowerAlphaName
    | -- | Input to a function -> must be of kind Type
      FunctionParameter
    | -- | The inferred kind of a definition must match its signature
      DefinitionKind (Qualified TypeName)
    | -- | Generic primitive constraint, e.g. "Unit must be of kind Type". This should be replaced with more specific origins where possible.
      PrimitiveConstraint Text
    | -- | The declaration of a user defined type
      UserDefinedTypeConstraint (Located (Qualified TypeName))
    deriving (Show, Eq, Generic)

instance Pretty ConstraintOrigin where
    pretty = \case
        FunctionApplication (New.Type loc _ _) (_, arg') -> "Application of type at" <+> pretty loc <+> "to argument of kind" <+> pretty (getAnn' arg')
        FunctionResult -> "Result of function application"
        ListElement -> "List element"
        RecordField f -> "Record field" <+> pretty f
        FunctionParameter -> "Function parameter"
        DefinitionKind n -> "Definition of" <+> pretty n
        PrimitiveConstraint s -> pretty s
        UserDefinedTypeConstraint n -> "User defined type" <+> pretty (n ^. unlocated)

originPositions :: ConstraintOrigin -> [(Position, Marker (Doc AnsiStyle))]
originPositions = \case
    FunctionApplication (New.Type loc _ _) _ -> [(sourceRegionToDiagnosePosition loc, This "Function application")]
    _ -> []

data InferState = InferState
    { env :: Map (Either (Qualified TypeName) (Unique LowerAlphaName)) KindVar
    -- ^ a mapping from type variables and named types to their kind variables
    , kindEnv :: Map (Qualified TypeName) ElaraKind
    -- ^ predefined kinds for named types
    , constraints :: [(ElaraKind, ElaraKind, ConstraintOrigin)]
    , substitution :: Map KindVar ElaraKind
    }
    deriving (Eq, Show, Generic)

type KindInfer r =
    ( State InferState :> r
    , Error KindInferError :> r
    , UniqueGen :> r
    , HasCallStack
    , QueryEffects r
    , Rock.Rock Elara.Query.Query :> r
    )

initialInferState :: InferState
initialInferState =
    InferState
        { kindEnv = primKindCheckContext
        , env = mempty
        , constraints = mempty
        , substitution = mempty
        }

data KindInferError
    = UnknownKind (Qualified TypeName) (Map (Qualified TypeName) ElaraKind)
    | UnboundVar (Unique LowerAlphaName) (Map (Either (Qualified TypeName) (Unique LowerAlphaName)) KindVar)
    | CannotUnify ElaraKind ElaraKind ConstraintOrigin
    | NotFunctionKind ElaraKind
    | OccursCheckFailed KindVar ElaraKind
    deriving (Generic, Show)

instance Pretty KindInferError

instance ReportableError KindInferError where
    report (UnknownKind name kinds) =
        writeReport $
            Err
                (Just "Unknown Kind of Type")
                ( vsep
                    [ "Unknown kind of type" <+> pretty name
                    , "We know kinds for:" <+> pretty (Map.keysSet kinds)
                    , pretty $ prettyCallStack callStack
                    ]
                )
                []
                []
    report (CannotUnify a b origin) =
        writeReport $
            Err
                (Just "Cannot Unify Kinds")
                (vsep ["Cannot unify kinds" <+> pretty a <+> "and" <+> pretty b, "Origin:" <+> pretty origin])
                (originPositions origin)
                []
    report (NotFunctionKind k) =
        writeReport $
            Err
                (Just "Not Function Kind")
                (vsep ["Expected a function kind, got" <+> pretty k])
                []
                []
    report (UnboundVar var env) =
        writeReport $
            Err
                (Just "Unbound Variable")
                ( vsep
                    [ "Unbound variable" <+> pretty var
                    , "Known variables:" <+> pretty (Map.keysSet env)
                    , pretty $ prettyCallStack callStack
                    ]
                )
                []
                []
    report (OccursCheckFailed var kind) =
        writeReport $
            Err
                (Just "Occurs Check Failed")
                (vsep ["Occurs check failed for" <+> pretty var <+> "and" <+> pretty kind])
                []
                []

declareTypeVar :: State InferState :> r => Unique LowerAlphaName -> KindVar -> Eff r ()
declareTypeVar var kindVar = modify (over (field @"env") (Map.insert (Right var) kindVar))

declareNamedType :: State InferState :> r => Qualified TypeName -> KindVar -> Eff r ()
declareNamedType name kindVar = modify (over (field @"env") (Map.insert (Left name) kindVar))

-- | Pass 1: Register a type name with a fresh kind variable (the "hole").
preRegisterType ::
    ( State InferState :> r
    , UniqueGen :> r
    , HasCallStack
    ) =>
    Qualified TypeName -> Eff r ()
preRegisterType name = do
    kindVar <- makeUniqueId

    declareNamedType name kindVar

elaborate ::
    KindInfer r =>
    Qualified TypeName ->
    [TypeVariable NewS.Shunted SourceRegion] ->
    NewS.ShuntedTypeDeclaration ->
    Eff r (KindVar, NewM.MidKindedTypeDeclaration)
elaborate tName tvs t = debugWith ("elaborate: " <> pretty tName) $ do
    varKinds <- for tvs $ \tv -> do
        kindVar <- makeUniqueId
        declareTypeVar (tv ^. unlocated) kindVar
        pure kindVar

    t' <- case t of
        New.Alias a -> do
            a' <- elaborateType a
            newEqualityConstraint (getAnn' a') TypeKind (DefinitionKind tName)
            pure (New.Alias a')
        New.ADT constructors -> do
            x <- for constructors $ \(n, conArgs) -> do
                conArgs' <- for conArgs $ \arg -> do
                    arg' <- elaborateType arg
                    newEqualityConstraint (getAnn' arg') TypeKind (DefinitionKind tName)
                    pure arg'
                pure (n, conArgs')
            pure (New.ADT x)

    -- We grab the kind variable of the data type
    -- so we can add a constraint on it.
    datatypeKindVar <- lookupNameKindVar tName (DefinitionKind tName)

    -- A type of the form `T a b c ... =` has the kind:
    -- `aKind -> bKind -> cKind -> ... -> Type`.
    -- We add that as a constraint.
    let kind = foldr (FunctionKind . VarKind) TypeKind varKinds
    newEqualityConstraint (VarKind datatypeKindVar) kind (DefinitionKind tName)

    pure (datatypeKindVar, t')

inferKind ::
    (HasCallStack, _) =>
    -- | The name of the type being inferred
    Qualified TypeName ->
    -- | The type variables of the type being inferred
    [TypeVariable NewS.Shunted SourceRegion] ->
    -- | The type declaration to infer the kind of
    NewS.ShuntedTypeDeclaration ->
    Eff r (ElaraKind, NewK.KindedTypeDeclaration)
inferKind name tvs t = do
    logDebug ("Inferring kind for type " <> pretty name)
    kindVar <- makeUniqueId
    declareNamedType name kindVar

    (kv, decl') <- elaborate name tvs t
    solveConstraints

    kind <- lookupKindVarInSubstitution kv
    logDebug ("Inferred kind for type " <> pretty name <> ": " <> pretty kind)
    body <- case decl' of
        New.Alias a -> do
            a' <- solveType a
            logDebug ("Inferred kind for alias " <> pretty name <> ": " <> pretty kind)
            pure (New.Alias a')
        New.ADT constructors -> do
            constructors' <- for constructors $ \(n, conArgs) -> do
                conArgs' <- for conArgs $ \(arg :: NewM.MidKindedType) -> do
                    solveType arg

                pure (n, conArgs')
            pure (New.ADT constructors')

    pure (kind, body)

inferTypeKind :: KindInfer r => NewS.ShuntedType -> Eff r NewK.KindedType
inferTypeKind t = do
    for_ (freeTypeVars t) $ \var -> do
        kindVar <- makeUniqueId
        declareTypeVar (var ^. unlocated) kindVar
    t' <- elaborateType t
    solveConstraints
    solveType t'

freeTypeVars :: NewS.ShuntedType -> Set (TypeVariable NewS.Shunted SourceRegion)
freeTypeVars (New.Type _ _ t) = go t
  where
    go = \case
        New.TVar v -> one v
        New.TFun a b -> freeTypeVars a <> freeTypeVars b
        New.TUnit -> mempty
        New.TApp a b -> freeTypeVars a <> freeTypeVars b
        New.TUserDefined _ -> mempty
        New.TRecord fields -> foldMap (freeTypeVars . snd) (toList fields)
        New.TList t' -> freeTypeVars t'
        New.TExtension v -> absurd v

lookupKindVarInSubstitution :: KindInfer r => KindVar -> Eff r ElaraKind
lookupKindVarInSubstitution var = do
    InferState{..} <- get
    case Map.lookup var substitution of
        Just kind -> pure kind
        Nothing -> pure (VarKind var)

lookupNameKindVar ::
    ( State InferState :> r
    , UniqueGen :> r
    , Error KindInferError :> r
    , QueryEffects r
    , Rock.Rock Elara.Query.Query :> r
    ) =>
    Qualified TypeName -> ConstraintOrigin -> Eff r KindVar
lookupNameKindVar name origin = do
    InferState{..} <- get
    case Map.lookup (Left name) env of
        Just kv -> pure kv
        Nothing ->
            case Map.lookup name kindEnv of
                Just kindVar -> do
                    newKindVar <- makeUniqueId
                    newEqualityConstraint (VarKind newKindVar) kindVar origin
                    pure newKindVar
                Nothing -> do
                    Rock.fetch (Elara.Query.KindOf name) ?:! throwError (UnknownKind name kindEnv)

-- | Find the kind variable of a type variable in the environment.
lookupVarKindVar :: KindInfer r => Unique LowerAlphaName -> Eff r KindVar
lookupVarKindVar var = do
    InferState{..} <- get
    case Map.lookup (Right var) env of
        Just kindVar -> pure kindVar
        Nothing -> throwError $ UnboundVar var env

lookupKindVarMaybe :: KindInfer r => Either (Qualified TypeName) (Unique LowerAlphaName) -> Eff r (Maybe KindVar)
lookupKindVarMaybe key = do
    InferState{..} <- get
    pure $ Map.lookup key env

newEqualityConstraint :: State InferState :> r => ElaraKind -> ElaraKind -> ConstraintOrigin -> Eff r ()
newEqualityConstraint a b origin = modify (over (field @"constraints") ((a, b, origin) :))

elaborateType :: KindInfer r => NewS.ShuntedType -> Eff r NewM.MidKindedType
elaborateType (New.Type loc () t') = do
    case t' of
        New.TVar var -> do
            kv <- lookupVarKindVar (var ^. unlocated)
            pure $ New.Type loc kv (New.TVar var)
        New.TUnit -> do
            kv <- makeUniqueId
            newEqualityConstraint (VarKind kv) TypeKind (PrimitiveConstraint "Unit must be of kind Type")
            pure $ New.Type loc kv New.TUnit
        New.TUserDefined name -> do
            kv <- lookupNameKindVar (name ^. unlocated) (UserDefinedTypeConstraint name)
            pure $ New.Type loc kv (New.TUserDefined name)
        New.TApp f arg -> do
            f' <- elaborateType f
            arg' <- elaborateType arg
            typeAppKindVar <- makeUniqueId

            newEqualityConstraint
                (getAnn' f')
                (FunctionKind (getAnn' arg') (VarKind typeAppKindVar))
                (FunctionApplication (New.Type loc () t') (arg, arg'))

            pure $ New.Type loc typeAppKindVar (New.TApp f' arg')
        New.TFun a b -> do
            a' <- elaborateType a
            b' <- elaborateType b
            res <- makeUniqueId
            newEqualityConstraint (getAnn' a') TypeKind FunctionParameter
            newEqualityConstraint (getAnn' b') TypeKind FunctionResult
            newEqualityConstraint (VarKind res) TypeKind FunctionResult

            pure $ New.Type loc res (New.TFun a' b')
        New.TList t'' -> do
            t''' <- elaborateType t''
            listKindVar <- makeUniqueId
            newEqualityConstraint (getAnn' t''') TypeKind ListElement
            newEqualityConstraint (VarKind listKindVar) TypeKind (PrimitiveConstraint "List is a Type")
            pure $ New.Type loc listKindVar (New.TList t''')
        New.TRecord fields -> do
            fields' <- for fields $ \(name, t'') -> do
                t''' <- elaborateType t''
                pure (name, t''')
            recordKindVar <- makeUniqueId
            traverseOf_
                (each % _2)
                (\k -> newEqualityConstraint (VarKind recordKindVar) (getAnn' k) (PrimitiveConstraint "Record is a Type"))
                fields'
            pure $ New.Type loc recordKindVar (New.TRecord fields')
        New.TExtension v -> absurd v

solveType :: KindInfer r => NewM.MidKindedType -> Eff r NewK.KindedType
solveType (New.Type loc kindVar t') = do
    kind' <- lookupKindVarInSubstitution kindVar
    case t' of
        New.TVar v -> do
            pure $ New.Type loc kind' (New.TVar v)
        New.TUnit -> pure $ New.Type loc kind' New.TUnit
        New.TUserDefined name -> pure $ New.Type loc kind' (New.TUserDefined name)
        New.TApp f arg -> do
            f' <- solveType f
            arg' <- solveType arg
            pure $ New.Type loc kind' (New.TApp f' arg')
        New.TFun a b -> do
            a' <- solveType a
            b' <- solveType b
            pure $ New.Type loc kind' (New.TFun a' b')
        New.TList t'' -> do
            t''' <- solveType t''
            pure $ New.Type loc kind' (New.TList t''')
        New.TRecord fields -> do
            fields' <- traverseOf (each % _2) solveType fields
            pure $ New.Type loc kind' (New.TRecord fields')
        New.TExtension v -> absurd v

getAnn' :: NewM.MidKindedType -> ElaraKind
getAnn' (New.Type _ kv _) = VarKind kv

solveConstraints ::
    ( State InferState :> r
    , Error KindInferError :> r
    , HasCallStack
    , UniqueGen :> r
    , QueryEffects r
    , Rock.Rock Elara.Query.Query :> r
    ) =>
    Eff r ()
solveConstraints = do
    constraint <- do
        InferState{..} <- get
        case constraints of
            [] -> pure Nothing
            (a, b, origin) : rest -> do
                modify (set (field @"constraints") rest)
                pure $ Just (a, b, origin)

    case constraint of
        -- If there are no constraints left, we're done
        Nothing -> pass
        -- If we have 2 kinds that are the same, we can remove the constraint
        Just (TypeKind, TypeKind, _) -> solveConstraints
        -- If we have 2 function kinds, we solve the constraints for the arguments and the result
        Just (FunctionKind k1 k2, FunctionKind k1' k2', origin) -> do
            newEqualityConstraint k1 k1' origin
            newEqualityConstraint k2 k2' origin
            solveConstraints

        -- instantiate a kind scheme
        Just (KindScheme vars kind, k, origin) -> do
            kind' <- instantiate kind vars
            newEqualityConstraint kind' k origin

            solveConstraints

        -- instantiate a kind scheme (reversed)
        Just (k, KindScheme vars kind, origin) -> do
            kind' <- instantiate kind vars
            newEqualityConstraint k kind' origin

            solveConstraints
        Just (VarKind v, k, _) -> do
            replaceInState v k
            solveConstraints
        Just (k, VarKind v, _) -> do
            replaceInState v k
            solveConstraints
        Just (k1@TypeKind, k2@FunctionKind{}, origin) -> throwError (CannotUnify k1 k2 origin)
        Just (k1@FunctionKind{}, k2@TypeKind, origin) -> throwError (CannotUnify k1 k2 origin)

instantiate :: UniqueGen :> r => ElaraKind -> [KindVar] -> Eff r ElaraKind
instantiate = foldM replaceKindVarWithFreshKindVar

replaceKindVarWithFreshKindVar :: UniqueGen :> r => ElaraKind -> KindVar -> Eff r ElaraKind
replaceKindVarWithFreshKindVar kind var = do
    kindvar <- makeUniqueId

    pure $
        transformOf
            plate
            ( \case
                VarKind v | v == var -> VarKind kindvar
                other -> other
            )
            kind

replaceInState :: KindInfer r => KindVar -> ElaraKind -> Eff r ()
replaceInState var kind = do
    occursCheck var kind

    s <- get @InferState
    let subst = transformOf plate (\case VarKind v | v == var -> kind; other -> other)
    let s' =
            s
                { kindEnv = Map.map subst s.kindEnv
                , constraints = s.constraints <&> \(k1, k2, o) -> (subst k1, subst k2, o)
                , substitution = Map.map subst s.substitution
                }

    put (s' & field @"substitution" %~ Map.insert var kind)

occursCheck :: KindInfer r => KindVar -> ElaraKind -> Eff r ()
occursCheck var kind
    | VarKind var == kind = pass -- unifying a var with itself is a no-op
    | var `Set.member` freeKindVars kind = throwError (OccursCheckFailed var kind)
    | otherwise = pass

freeKindVars :: ElaraKind -> Set KindVar
freeKindVars = \case
    TypeKind -> mempty
    FunctionKind l r -> freeKindVars l <> freeKindVars r
    VarKind v -> one v
    KindScheme vars k -> freeKindVars k `Set.difference` Set.fromList vars
