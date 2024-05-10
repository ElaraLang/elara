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
module Elara.Data.Kind.Infer where

import Control.Monad (foldM)
import Data.Data (Data)
import Data.Generics.Wrapped
import Data.Map qualified as Map
import Data.Traversable (for)
import Elara.AST.Generic
import Elara.AST.Generic.Common
import Elara.AST.Kinded
import Elara.AST.Name (LowerAlphaName, Qualified, TypeName)
import Elara.AST.Region (Located, unlocated)
import Elara.AST.Select
import Elara.AST.Shunted
import Elara.Data.Kind
import Elara.Data.Pretty
import Elara.Data.Unique (Unique, UniqueGen, UniqueId, makeUniqueId)
import Elara.Error
import Elara.Prim (primKindCheckContext)
import Elara.Utils (uncurry3)
import Error.Diagnose
import Optics (set, traverseOf_, universeOf)
import Polysemy (Member, Sem)
import Polysemy.Error
import Polysemy.State
import TODO (todo)

type KindVar = UniqueId
type TypeVar = Unique LowerAlphaName
data InferState = InferState
    { env :: Map (Either (Qualified TypeName) TypeVar) KindVar
    , kindEnv :: Map (Qualified TypeName) ElaraKind
    , constraints :: [(ElaraKind, ElaraKind)]
    , substitution :: Map KindVar ElaraKind
    }
    deriving (Eq, Show, Generic, Data)

type KindInfer r = (Member (State InferState) r, Member (Error KindInferError) r, Member UniqueGen r, HasCallStack)

initialInferState :: InferState
initialInferState =
    InferState
        { kindEnv = primKindCheckContext
        , env = mempty
        , constraints = mempty
        , substitution = mempty
        }

data KindInferError
    = HasCallStack => UnknownKind (Qualified TypeName) (Map (Qualified TypeName) ElaraKind)
    | HasCallStack => UnboundVar TypeVar (Map (Either (Qualified TypeName) TypeVar) KindVar)
    | CannotUnify ElaraKind ElaraKind
    | NotFunctionKind ElaraKind
    | OccursCheckFailed KindVar ElaraKind

deriving instance Show KindInferError

instance ReportableError KindInferError where
    report (UnknownKind name kinds) =
        writeReport $
            Err
                (Just "Unknown Kind")
                ( vsep
                    [ "Unknown kind" <+> pretty name
                    , "Known kinds:" <+> pretty (Map.keysSet kinds)
                    , pretty $ prettyCallStack callStack
                    ]
                )
                []
                []
    report (CannotUnify a b) =
        writeReport $
            Err
                (Just "Cannot Unify Kinds")
                (vsep ["Cannot unify kinds" <+> pretty a <+> "and" <+> pretty b])
                []
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

declareTypeVar :: Member (State InferState) r => TypeVar -> KindVar -> Sem r ()
declareTypeVar var kindVar = modify (over #env (Map.insert (Right var) kindVar))

declareNamedType :: Member (State InferState) r => Qualified TypeName -> KindVar -> Sem r ()
declareNamedType name kindVar = modify (over #env (Map.insert (Left name) kindVar))

elaborate ::
    KindInfer r =>
    Qualified TypeName ->
    [Located (Unique LowerAlphaName)] ->
    ShuntedTypeDeclaration ->
    Sem r (KindVar, MidKindedTypeDeclaration)
elaborate tName tvs t = do
    varKinds <- for tvs $ \tv -> do
        kindVar <- makeUniqueId
        declareTypeVar (tv ^. unlocated) kindVar
        pure kindVar

    t' <- case t of
        Alias a -> do
            todo
        ADT constructors -> do
            x <- for constructors $ \(n, conArgs) -> do
                conArgs' <- for conArgs $ \arg -> do
                    arg' <- elaborateType arg
                    newEqualityConstraint (getAnn' arg') TypeKind
                    pure arg'
                pure (n, conArgs')
            pure (ADT @MidKinded x)

    -- We grab the kind variable of the data type
    -- so we can add a constraint on it.
    datatypeKindVar <- lookupNameKindVar tName

    -- A type of the form `T a b c ... =` has the kind:
    -- `aKind -> bKind -> cKind -> ... -> Type`.
    -- We add that as a constraint.
    let kind = foldr (FunctionKind . VarKind) TypeKind varKinds
    newEqualityConstraint (VarKind datatypeKindVar) kind

    pure (datatypeKindVar, t')

infer :: KindInfer r => Map (Qualified TypeName) ElaraKind -> [(Qualified TypeName, [Located TypeVar], ShuntedTypeDeclaration)] -> Sem r [(ElaraKind, KindedTypeDeclaration)]
infer kindEnv decls = do
    modify (set #kindEnv kindEnv)
    for_ decls $ \(name, _, _) -> do
        kindvar <- makeUniqueId
        declareNamedType name kindvar

    decls' <- traverse (uncurry3 elaborate) decls
    solveConstraints

    for decls' $ \(kindVar, body) -> do
        kind <- lookupKindVarInSubstitution kindVar
        body' <- case body of
            ADT constructors -> do
                constructors' <- for constructors $ \(n, conArgs) -> do
                    conArgs' <- for conArgs $ \(arg :: MidKindedType) -> do
                        solveType arg

                    pure (n, conArgs')
                pure (ADT @Kinded constructors')
            Alias a -> do
                todo
        pure (kind, body')

inferKind :: (Member (State InferState) r, Member (Error KindInferError) r, Member UniqueGen r) => Qualified TypeName -> [Located TypeVar] -> ShuntedTypeDeclaration -> Sem r (ElaraKind, KindedTypeDeclaration)
inferKind name tvs t = do
    kindVar <- makeUniqueId
    declareNamedType name kindVar

    (kv, decl') <- elaborate name tvs t
    solveConstraints

    kind <- lookupKindVarInSubstitution kv
    body <- case decl' of
        Alias a -> do
            todo
        ADT constructors -> do
            constructors' <- for constructors $ \(n, conArgs) -> do
                conArgs' <- for conArgs $ \(arg :: MidKindedType) -> do
                    solveType arg

                pure (n, conArgs')
            pure (ADT @Kinded constructors')

    pure (kind, body)

inferTypeKind :: KindInfer r => ShuntedType -> Sem r KindedType
inferTypeKind t = do
    for_ (freeTypeVars t) $ \var -> do
        kindVar <- makeUniqueId
        declareTypeVar (var ^. unlocated) kindVar
    t' <- elaborateType t
    solveConstraints
    solveType t'

lookupKindVarInSubstitution :: KindInfer r => KindVar -> Sem r ElaraKind
lookupKindVarInSubstitution var = do
    InferState{..} <- get
    case Map.lookup var substitution of
        Just kind -> pure kind
        Nothing -> pure (VarKind var)

lookupNameKindVar :: (Member (State InferState) r, Member UniqueGen r, Member (Error KindInferError) r) => Qualified TypeName -> Sem r KindVar
lookupNameKindVar name = do
    InferState{..} <- get
    case Map.lookup name kindEnv of
        Just kindVar -> do
            newKindVar <- makeUniqueId
            newEqualityConstraint (VarKind newKindVar) kindVar
            pure newKindVar
        Nothing ->
            maybe (throw $ UnknownKind name kindEnv) pure (Map.lookup (Left name) env)

-- | Find the kind variable of a type variable in the environment.
lookupVarKindVar :: KindInfer r => TypeVar -> Sem r KindVar
lookupVarKindVar var = do
    InferState{..} <- get
    case Map.lookup (Right var) env of
        Just kindVar -> pure kindVar
        Nothing -> throw $ UnboundVar var env

newEqualityConstraint :: Member (State InferState) r => ElaraKind -> ElaraKind -> Sem r ()
newEqualityConstraint a b = modify (over #constraints ((a, b) :))

elaborateType :: KindInfer r => ShuntedType -> Sem r MidKindedType
elaborateType (Type (t :: Located (Type' 'Shunted), NoFieldValue)) = do
    case t ^. unlocated of
        TypeVar var -> do
            kv <- lookupVarKindVar (var ^. unlocated)
            pure $ Type (TypeVar var <$ t, kv)
        UnitType -> do
            kv <- makeUniqueId
            newEqualityConstraint (VarKind kv) TypeKind
            pure $ Type (UnitType <$ t, kv)
        UserDefinedType name -> do
            kv <- lookupNameKindVar (name ^. unlocated)
            pure $ Type (UserDefinedType name <$ t, kv)
        TypeConstructorApplication f arg -> do
            f' <- elaborateType f
            arg' <- elaborateType arg
            typeAppKindVar <- makeUniqueId

            newEqualityConstraint
                (getAnn' f')
                (FunctionKind (getAnn' arg') (VarKind typeAppKindVar))

            pure $ Type (TypeConstructorApplication f' arg' <$ t, typeAppKindVar)
        FunctionType a b -> do
            a' <- elaborateType a
            b' <- elaborateType b
            res <- makeUniqueId
            newEqualityConstraint (getAnn' a') TypeKind
            newEqualityConstraint (getAnn' b') TypeKind
            newEqualityConstraint (VarKind res) TypeKind

            pure $ Type (FunctionType a' b' <$ t, res)
        TupleType ts -> do
            ts' <- for ts elaborateType
            tupleKindVar <- makeUniqueId
            traverseOf_ (each % _Unwrapped % _2) (newEqualityConstraint (VarKind tupleKindVar) . VarKind) ts'
            pure $ Type (TupleType ts' <$ t, tupleKindVar)
        ListType t' -> do
            t'' <- elaborateType t'
            listKindVar <- makeUniqueId
            newEqualityConstraint (getAnn' t'') TypeKind
            newEqualityConstraint (VarKind listKindVar) TypeKind
            pure $ Type (ListType t'' <$ t, listKindVar)
        RecordType fields -> do
            fields' <- for fields $ \(name, t') -> do
                t'' <- elaborateType t'
                pure (name, t'')
            recordKindVar <- makeUniqueId
            traverseOf_ (each % _2 % _Unwrapped % _2) (newEqualityConstraint (VarKind recordKindVar) . VarKind) fields'
            pure $ Type (RecordType fields' <$ t, recordKindVar)

solveType :: KindInfer r => MidKindedType -> Sem r KindedType
solveType (Type (t, kindVar)) = do
    kind' <- lookupKindVarInSubstitution kindVar
    case t ^. unlocated of
        TypeVar v -> do
            pure $ Type (TypeVar v <$ t, kind')
        UnitType -> pure $ Type (UnitType <$ t, kind')
        UserDefinedType name -> pure $ Type (UserDefinedType name <$ t, kind')
        TypeConstructorApplication f arg -> do
            f' <- solveType f
            arg' <- solveType arg
            pure $ Type (TypeConstructorApplication f' arg' <$ t, kind')
        FunctionType a b -> do
            a' <- solveType a
            b' <- solveType b
            pure $ Type (FunctionType a' b' <$ t, kind')
        TupleType ts -> do
            ts' <- traverse solveType ts
            pure $ Type (TupleType ts' <$ t, kind')
        ListType t' -> do
            t'' <- solveType t'
            pure $ Type (ListType t'' <$ t, kind')
        RecordType fields -> do
            fields' <- traverseOf (each % _2) solveType fields
            pure $ Type (RecordType fields' <$ t, kind')

getAnn :: KindedType -> ElaraKind
getAnn (Type (_, b)) = b

getAnn' :: MidKindedType -> ElaraKind
getAnn' (Type (_, b)) = VarKind b

solveConstraints ::
    (Member (State InferState) r, Member (Error KindInferError) r, HasCallStack, Member UniqueGen r) =>
    Sem r ()
solveConstraints = do
    constraint <- do
        InferState{..} <- get
        case constraints of
            [] -> pure Nothing
            (a, b) : rest -> do
                modify (set #constraints rest)
                pure $ Just (a, b)

    case constraint of
        -- If there are no constraints left, we're done
        Nothing -> pass
        -- If we have 2 kinds that are the same, we can remove the constraint
        Just (TypeKind, TypeKind) -> solveConstraints
        -- If we have 2 function kinds, we solve the constraints for the arguments and the result
        Just (FunctionKind k1 k2, FunctionKind k1' k2') -> do
            newEqualityConstraint k1 k1'
            newEqualityConstraint k2 k2'
            solveConstraints

        -- instantiate a kind scheme
        Just (KindScheme vars kind, k) -> do
            kind' <- instantiate kind vars
            newEqualityConstraint kind' k

            solveConstraints

        -- instantiate a kind scheme (reversed)
        Just (k, KindScheme vars kind) -> do
            kind' <- instantiate kind vars
            newEqualityConstraint k kind'

            solveConstraints
        Just (VarKind v, k) -> do
            replaceInState v k
            solveConstraints
        Just (k, VarKind v) -> do
            replaceInState v k
            solveConstraints
        Just (k1@TypeKind, k2@FunctionKind{}) -> throw (CannotUnify k1 k2)
        Just (k1@FunctionKind{}, k2@TypeKind) -> throw (CannotUnify k1 k2)

instantiate :: Member UniqueGen r => ElaraKind -> [KindVar] -> Sem r ElaraKind
instantiate = foldM replaceKindVarWithFreshKindVar

replaceKindVarWithFreshKindVar :: Member UniqueGen r => ElaraKind -> KindVar -> Sem r ElaraKind
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

replaceInState :: KindInfer r => KindVar -> ElaraKind -> Sem r ()
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

occursCheck :: KindInfer r => KindVar -> ElaraKind -> Sem r ()
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
            throw (OccursCheckFailed var kind)
