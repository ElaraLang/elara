{-# LANGUAGE OverloadedLabels #-}
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
) where

import Control.Monad (foldM)
import Data.Data (Data)
import Data.Generics.Wrapped
import Data.Map qualified as Map
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Elara.AST.Generic
import Elara.AST.Generic.Common
import Elara.AST.Kinded
import Elara.AST.Name (LowerAlphaName, Qualified, TypeName)
import Elara.AST.Region (HasSourceRegion (sourceRegion), Located, sourceRegionToDiagnosePosition, unlocated)
import Elara.AST.Select
import Elara.AST.Shunted
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
import Optics (set, traverseOf_, universeOf)
import Rock qualified

{- | Tracks the origin of a kind constraint for better error messages.
This represents the "why" of why a constraint was added.
-}
data ConstraintOrigin
    = -- | A (type-level) function was applied to an argument
      FunctionApplication
        { functionType :: Located ShuntedType'
        -- ^ The function being applied
        , functionArg :: (ShuntedType, MidKindedType)
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
    deriving (Show, Eq, Data, Generic)

instance Pretty ConstraintOrigin where
    pretty = \case
        FunctionApplication f arg -> "Application of type" <+> pretty (f ^. unlocated) <+> "to argument of kind" <+> pretty (getAnn' (snd arg))
        FunctionResult -> "Result of function application"
        ListElement -> "List element"
        RecordField f -> "Record field" <+> pretty f
        FunctionParameter -> "Function parameter"
        DefinitionKind n -> "Definition of" <+> pretty n
        PrimitiveConstraint s -> pretty s
        UserDefinedTypeConstraint n -> "User defined type" <+> pretty (n ^. unlocated)

originPositions :: ConstraintOrigin -> [(Position, Marker (Doc AnsiStyle))]
originPositions = \case
    FunctionApplication f _ -> [(sourceRegionToDiagnosePosition (f ^. sourceRegion), This "Function application")]
    _ -> []

data InferState = InferState
    { env :: Map (Either (Qualified TypeName) TypeVar) KindVar
    -- ^ a mapping from type variables and named types to their kind variables
    , kindEnv :: Map (Qualified TypeName) ElaraKind
    -- ^ predefined kinds for named types
    , constraints :: [(ElaraKind, ElaraKind, ConstraintOrigin)]
    , substitution :: Map KindVar ElaraKind
    }
    deriving (Eq, Show, Generic, Data)

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
    | UnboundVar TypeVar (Map (Either (Qualified TypeName) TypeVar) KindVar)
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

declareTypeVar :: State InferState :> r => TypeVar -> KindVar -> Eff r ()
declareTypeVar var kindVar = modify (over #env (Map.insert (Right var) kindVar))

declareNamedType :: State InferState :> r => Qualified TypeName -> KindVar -> Eff r ()
declareNamedType name kindVar = modify (over #env (Map.insert (Left name) kindVar))

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
    [Located (Unique LowerAlphaName)] ->
    ShuntedTypeDeclaration ->
    Eff r (KindVar, MidKindedTypeDeclaration)
elaborate tName tvs t = debugWith ("elaborate: " <> pretty tName) $ do
    varKinds <- for tvs $ \tv -> do
        kindVar <- makeUniqueId
        declareTypeVar (tv ^. unlocated) kindVar
        pure kindVar

    t' <- case t of
        Alias a -> do
            a' <- elaborateType a
            newEqualityConstraint (getAnn' a') TypeKind (DefinitionKind tName)
            pure (Alias a')
        ADT constructors -> do
            x <- for constructors $ \(n, conArgs) -> do
                conArgs' <- for conArgs $ \arg -> do
                    arg' <- elaborateType arg
                    newEqualityConstraint (getAnn' arg') TypeKind (DefinitionKind tName)
                    pure arg'
                pure (n, conArgs')
            pure (ADT @MidKinded x)

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
    [Located TypeVar] ->
    -- | The type declaration to infer the kind of
    ShuntedTypeDeclaration ->
    Eff r (ElaraKind, KindedTypeDeclaration)
inferKind name tvs t = do
    logDebug ("Inferring kind for type " <> pretty name)
    kindVar <- makeUniqueId
    declareNamedType name kindVar

    (kv, decl') <- elaborate name tvs t
    solveConstraints

    kind <- lookupKindVarInSubstitution kv
    logDebug ("Inferred kind for type " <> pretty name <> ": " <> pretty kind)
    body <- case decl' of
        Alias a -> do
            a' <- solveType a
            logDebug ("Inferred kind for alias " <> pretty name <> ": " <> pretty kind)
            pure (Alias a')
        ADT constructors -> do
            constructors' <- for constructors $ \(n, conArgs) -> do
                conArgs' <- for conArgs $ \(arg :: MidKindedType) -> do
                    solveType arg

                pure (n, conArgs')
            pure (ADT @Kinded constructors')

    pure (kind, body)

inferTypeKind :: KindInfer r => ShuntedType -> Eff r KindedType
inferTypeKind t = do
    for_ (freeTypeVars t) $ \var -> do
        kindVar <- makeUniqueId
        declareTypeVar (var ^. unlocated) kindVar
    t' <- elaborateType t
    solveConstraints
    solveType t'

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
lookupVarKindVar :: KindInfer r => TypeVar -> Eff r KindVar
lookupVarKindVar var = do
    InferState{..} <- get
    case Map.lookup (Right var) env of
        Just kindVar -> pure kindVar
        Nothing -> throwError $ UnboundVar var env

lookupKindVarMaybe :: KindInfer r => Either (Qualified TypeName) TypeVar -> Eff r (Maybe KindVar)
lookupKindVarMaybe key = do
    InferState{..} <- get
    pure $ Map.lookup key env

newEqualityConstraint :: State InferState :> r => ElaraKind -> ElaraKind -> ConstraintOrigin -> Eff r ()
newEqualityConstraint a b origin = modify (over #constraints ((a, b, origin) :))

elaborateType :: KindInfer r => ShuntedType -> Eff r MidKindedType
elaborateType (Type (t :: Located (Type' Shunted), NoFieldValue)) = do
    case t ^. unlocated of
        TypeVar var -> do
            kv <- lookupVarKindVar (var ^. unlocated)
            pure $ Type (TypeVar var <$ t, kv)
        UnitType x -> do
            kv <- makeUniqueId
            newEqualityConstraint (VarKind kv) TypeKind (PrimitiveConstraint "Unit must be of kind Type")
            pure $ Type (UnitType x <$ t, kv)
        UserDefinedType name -> do
            kv <- lookupNameKindVar (name ^. unlocated) (UserDefinedTypeConstraint name)
            pure $ Type (UserDefinedType name <$ t, kv)
        TypeConstructorApplication f arg -> do
            f' <- elaborateType f
            arg' <- elaborateType arg
            typeAppKindVar <- makeUniqueId

            newEqualityConstraint
                (getAnn' f')
                (FunctionKind (getAnn' arg') (VarKind typeAppKindVar))
                (FunctionApplication t (arg, arg'))

            pure $ Type (TypeConstructorApplication f' arg' <$ t, typeAppKindVar)
        FunctionType a b -> do
            a' <- elaborateType a
            b' <- elaborateType b
            res <- makeUniqueId
            newEqualityConstraint (getAnn' a') TypeKind FunctionParameter
            newEqualityConstraint (getAnn' b') TypeKind FunctionResult
            newEqualityConstraint (VarKind res) TypeKind FunctionResult

            pure $ Type (FunctionType a' b' <$ t, res)
        ListType t' -> do
            t'' <- elaborateType t'
            listKindVar <- makeUniqueId
            newEqualityConstraint (getAnn' t'') TypeKind ListElement
            newEqualityConstraint (VarKind listKindVar) TypeKind (PrimitiveConstraint "List is a Type")
            pure $ Type (ListType t'' <$ t, listKindVar)
        RecordType fields -> do
            fields' <- for fields $ \(name, t') -> do
                t'' <- elaborateType t'
                pure (name, t'')
            recordKindVar <- makeUniqueId
            traverseOf_
                (each % _2 % _Unwrapped % _2)
                (\k -> newEqualityConstraint (VarKind recordKindVar) (VarKind k) (PrimitiveConstraint "Record is a Type"))
                fields'
            pure $ Type (RecordType fields' <$ t, recordKindVar)

solveType :: KindInfer r => MidKindedType -> Eff r KindedType
solveType (Type (t, kindVar)) = do
    kind' <- lookupKindVarInSubstitution kindVar
    case t ^. unlocated of
        TypeVar v -> do
            pure $ Type (TypeVar v <$ t, kind')
        UnitType x -> pure $ Type (UnitType x <$ t, kind')
        UserDefinedType name -> pure $ Type (UserDefinedType name <$ t, kind')
        TypeConstructorApplication f arg -> do
            f' <- solveType f
            arg' <- solveType arg
            pure $ Type (TypeConstructorApplication f' arg' <$ t, kind')
        FunctionType a b -> do
            a' <- solveType a
            b' <- solveType b
            pure $ Type (FunctionType a' b' <$ t, kind')
        ListType t' -> do
            t'' <- solveType t'
            pure $ Type (ListType t'' <$ t, kind')
        RecordType fields -> do
            fields' <- traverseOf (each % _2) solveType fields
            pure $ Type (RecordType fields' <$ t, kind')

getAnn' :: MidKindedType -> ElaraKind
getAnn' (Type (_, b)) = VarKind b

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
                modify (set #constraints rest)
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
            gplate
            ( \case
                VarKind v | v == var -> VarKind kindvar
                other -> other
            )
            kind

replaceInState :: KindInfer r => KindVar -> ElaraKind -> Eff r ()
replaceInState var kind = do
    occursCheck var kind

    s <- get @InferState
    let s' =
            transformOf'
                gplate
                ( \case
                    VarKind v | v == var -> kind
                    other -> other
                )
                s

    put (s' & #substitution %~ Map.insert var kind)

occursCheck :: KindInfer r => KindVar -> ElaraKind -> Eff r ()
occursCheck var kind =
    if VarKind var == kind || null [() | VarKind v <- universeOf gplate kind, var == v]
        then pass
        else do
            -- We try to find the type of the kind variable by doing reverse lookup,
            -- but this might not succeed before the kind variable might be generated
            -- during constraint solving.
            -- We might be able to find the type if we look at the substitution as well,
            -- but for now lets leave it at this "best effort" attempt.
            -- reverseEnv :: [(KindVar, Either (Qualified TypeName) TypeVar)] <- map swap . toList . (view #env) <$> get
            -- let typ = lookup var reverseEnv
            throwError (OccursCheckFailed var kind)
