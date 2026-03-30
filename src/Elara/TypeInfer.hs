{-# LANGUAGE BlockArguments #-}

module Elara.TypeInfer where

import Data.Generics.Product (HasType (typed))
import Data.Graph (SCC, flattenSCC)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Local
import Effectful.Writer.Static.Local (runWriter)
import Elara.AST.Module qualified as NewModule
import Elara.AST.Name (LowerAlphaName, ModuleName, Name (..), NameLike (nameText), Qualified (..), ToName (..), TypeName, VarName, unqualified)
import Elara.AST.Phase (NoExtension (..))
import Elara.AST.PhaseCoerce (PhaseCoerce (..))
import Elara.AST.Phases.Kinded (KindedType)
import Elara.AST.Phases.Kinded qualified as NewK
import Elara.AST.Phases.Renamed (TypedLambdaParam (..))
import Elara.AST.Phases.Shunted (Shunted)
import Elara.AST.Phases.Shunted qualified as NewS
import Elara.AST.Phases.Typed (Typed, TypedDeclaration, TypedExpr, TypedExpr')
import Elara.AST.Phases.Typed qualified as NewT
import Elara.AST.Region (HasSourceRegion (..), Located (Located), SourceRegion, unlocated, withLocationOf)
import Elara.AST.Types qualified as New
import Elara.AST.VarRef
import Elara.Data.Kind (ElaraKind, KindVar)
import Elara.Data.Kind.Infer (KindInferError, inferKind, inferTypeKind, initialInferState, lookupKindVarMaybe)
import Elara.Data.Kind.Infer qualified as Kind
import Elara.Data.Pretty
import Elara.Data.Unique (Unique)
import Elara.Data.Unique.Effect
import Elara.Error (runErrorOrReport)
import Elara.Logging (StructuredDebug, debug, debugWith, debugWithResult, logDebug, logDebugWith)
import Elara.Query (Query (..), QueryType (..), SupportsQuery)
import Elara.Query.Effects
import Elara.Rules.Generic ()
import Elara.SCC.Type (SCCKey, sccKeyToSCC)
import Elara.Shunt ()
import Elara.Shunt.Error (ShuntError)
import Elara.TypeInfer.ConstraintGeneration
import Elara.TypeInfer.Context (emptyContextStack)
import Elara.TypeInfer.Convert (TypeConvertError, astTypeToGeneralisedInferType, astTypeToInferType, astTypeToInferTypeWithKind)
import Elara.TypeInfer.Environment (InferError, TypeEnvKey (..), TypeEnvironment, addType', emptyLocalTypeEnvironment, emptyTypeEnvironment)
import Elara.TypeInfer.Error (UnifyError (..))
import Elara.TypeInfer.Ftv (Fuv (..))
import Elara.TypeInfer.Generalise
import Elara.TypeInfer.Monad
import Elara.TypeInfer.Type (Constraint (..), Monotype (..), Polytype (..), Substitutable (..), Substitution (..), Type (..), TypeVariable (..), monotypeLoc, simpleEquality, typeLoc)
import Elara.TypeInfer.Unique (UniqueTyVar, makeUniqueTyVar)
import Optics (over, view)
import Relude.Extra (bimapF, fmapToSnd, secondF)
import Rock qualified

-- PhaseCoerce instances for Shunted → Typed (Exposing/Import type families are identical)
instance PhaseCoerce (NewModule.Exposing SourceRegion Shunted) (NewModule.Exposing SourceRegion Typed)

instance PhaseCoerce (NewModule.Exposition SourceRegion Shunted) (NewModule.Exposition SourceRegion Typed)

instance PhaseCoerce (NewModule.Import SourceRegion Shunted) (NewModule.Import SourceRegion Typed)

instance PhaseCoerce (NewModule.Import' SourceRegion Shunted) (NewModule.Import' SourceRegion Typed)

type InferPipelineEffects r =
    ( StructuredDebug :> r
    , State Kind.InferState :> r
    , UniqueGen :> r
    , Error (UnifyError SourceRegion) :> r
    , Error TypeConvertError :> r
    , Error KindInferError :> r
    , Infer SourceRegion r
    )

runGetTypeCheckedModuleQuery ::
    SupportsQuery QueryModuleByName Shunted =>
    ModuleName ->
    Eff
        ( ConsQueryEffects
            '[ Rock.Rock Elara.Query.Query
             ]
        )
        (NewModule.Module SourceRegion Typed)
runGetTypeCheckedModuleQuery mn = do
    shunted <- runErrorOrReport @ShuntError $ Rock.fetch $ Elara.Query.ModuleByName @Shunted mn
    r <- runInferEffects $ evalState initialInferState (inferModule shunted)
    pure (fst r)

-- | Run the 'TypeOf' query to get the type of a term or data constructor
runTypeOfQuery ::
    forall loc.
    loc ~ SourceRegion =>
    (SupportsQuery QueryRequiredDeclarationByName Shunted, SupportsQuery QueryModuleByName Shunted) =>
    TypeEnvKey loc ->
    Eff
        ( ConsQueryEffects
            '[Rock.Rock Elara.Query.Query]
        )
        (Type loc)
runTypeOfQuery key = runErrorOrReport @(InferError loc) $
    runErrorOrReport @(UnifyError loc) $
        runErrorOrReport @KindInferError $
            runErrorOrReport @TypeConvertError $
                evalState emptyLocalTypeEnvironment $
                    evalState initialInferState $
                        evalState emptyTypeEnvironment $
                            runReader emptyContextStack $
                                case key of
                                    TermVarKey varName -> logDebugWith ("TypeOf: " <> pretty varName) $ do
                                        sccs <- Rock.fetch $ GetSCCsOf varName
                                        logDebug $ "SCCs for " <> pretty varName <> ": " <> pretty (fmap flattenSCC sccs)
                                        -- Infer dependencies first to populate the environment
                                        for_ sccs seedSCC
                                        for_ sccs inferSCC
                                        -- Read from the environment (now populated) without re-querying
                                        lookupType (TermVarKey varName)
                                    DataConKey con -> do
                                        seedConstructorsFor (qualifier con)
                                        -- Read from the environment (now populated) without re-querying
                                        lookupType (DataConKey con)

runKindOfQuery :: SupportsQuery QueryModuleByName Shunted => Qualified TypeName -> Eff (ConsQueryEffects (Rock.Rock Query : r)) (Maybe KindVar)
runKindOfQuery qtn = fmap fst $ runInferEffects $ evalState initialInferState $ do
    logDebug $ "runKindOfQuery: " <> pretty qtn
    -- First, try to look up the kind variable directly
    -- it might be a primitive type or already inferred
    lookupKindVarMaybe (Left qtn) >>= \case
        Just kindVar -> pure (Just kindVar)
        Nothing -> do
            logDebug $ "Kind not found for " <> pretty qtn <> ", seeding whole module"
            -- if we can't find it, seed the whole module and try again
            seedConstructorsFor (qualifier qtn)

            -- in theory it should be here now...
            lookupKindVarMaybe (Left qtn)

runGetTypeAliasQuery ::
    forall r.
    SupportsQuery QueryModuleByName Shunted =>
    Qualified TypeName ->
    Eff (ConsQueryEffects (Rock.Rock Query : r)) (Maybe ([UniqueTyVar], Type SourceRegion))
runGetTypeAliasQuery name = do
    let modName = qualifier name
    mod <- runErrorOrReport @ShuntError $ Rock.fetch $ Elara.Query.ModuleByName @Shunted modName

    let NewModule.Module _ m' = mod
    let targetTypeName = name ^. unqualified
    let found = find (matchesTypeName targetTypeName) m'.moduleDeclarations

    case found of
        Just (New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body'))) -> do
            case body' of
                New.TypeDeclarationBody _ typeVars (New.Alias body) _ NoExtension _ -> do
                    res <- runInferEffects $ evalState initialInferState $ do
                        -- infer the kind of the alias
                        (_, typedDeclBody) <- inferKind name typeVars (New.Alias body)

                        case typedDeclBody of
                            New.Alias typedBody -> do
                                inferBody <- astTypeToInferType typedBody

                                let uVars = fmap createTypeVar typeVars

                                pure (Just (uVars, Lifted inferBody))
                            _ -> pure Nothing

                    -- Extract the result from the inference pipeline
                    pure (fst res)
                _ -> pure Nothing
        Nothing -> pure Nothing
  where
    matchesTypeName :: TypeName -> New.Declaration SourceRegion Shunted -> Bool
    matchesTypeName tn (New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body'))) =
        case body' of
            New.TypeDeclarationBody n _ _ _ _ _ -> (n ^. (unlocated % unqualified)) == tn
            _ -> False

seedConstructorsFor :: (SupportsQuery QueryModuleByName Shunted, HasCallStack, _) => ModuleName -> Eff r ()
seedConstructorsFor moduleName = debugWith ("seedConstructorsFor: " <> pretty moduleName) $ do
    -- Fetch all declarations in the module
    mod <- runErrorOrReport @ShuntError $ Rock.fetch $ Elara.Query.ModuleByName @Shunted moduleName
    let NewModule.Module _ m' = mod

    -- Extract type declarations
    let typeDecls = mapMaybe extractTypeDecl m'.moduleDeclarations

    for_ typeDecls $ \(name, _, _, _) -> Kind.preRegisterType (name ^. unlocated)

    -- For each type declaration, add its constructors to the environment
    for_ typeDecls $ \(name, typeVars, declBody, _anns) -> debugWith ("Seeding declaration: " <> pretty name) $ do
        (_, decl') <- inferKind (name ^. unlocated) typeVars declBody
        case decl' of
            New.Alias t -> do
                logDebug $ "Seeding alias type: " <> pretty name
                _ <- astTypeToInferType t
                pass -- we don't need to do anything with an alias i think
            New.ADT ctors -> do
                let tyVars' = fmap createTypeVar typeVars
                let typeConstructorType = TypeConstructor (name ^. sourceRegion) (name ^. unlocated) (fmap (TypeVar (name ^. sourceRegion) . UnificationVar) tyVars')

                let inferCtor (ctorName, t :: [KindedType]) = do
                        t' <- traverse astTypeToInferTypeWithKind t
                        let ctorType =
                                foldr (Function (name ^. sourceRegion) . fst) typeConstructorType t'
                        addType' (DataConKey (ctorName ^. unlocated)) (Polytype (Forall (name ^. sourceRegion) tyVars' (EmptyConstraint (monotypeLoc ctorType)) ctorType))

                        pure (ctorName, t')

                for_ ctors inferCtor
  where
    extractTypeDecl :: New.Declaration SourceRegion Shunted -> Maybe (Located (Qualified TypeName), [Located (Unique LowerAlphaName)], New.TypeDeclaration SourceRegion Shunted, [New.Annotation SourceRegion Shunted])
    extractTypeDecl (New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body'))) =
        case body' of
            New.TypeDeclarationBody name typeVars declBody _ _ anns -> Just (name, typeVars, declBody, anns)
            _ -> Nothing

runInferEffects ::
    forall r a loc.
    Pretty loc =>
    QueryEffects r =>
    Rock.Rock Query :> r =>
    Eq loc =>
    loc ~ SourceRegion =>
    Eff
        ( InferEffectsCons
            loc
            ( Error (UnifyError loc)
                ': Error KindInferError
                ': Error TypeConvertError
                ': r
            )
        )
        a ->
    Eff r (a, Constraint loc)
runInferEffects =
    runErrorOrReport @(InferError _)
        . runErrorOrReport @(UnifyError _)
        . runErrorOrReport @KindInferError
        . runErrorOrReport @TypeConvertError
        . evalState emptyTypeEnvironment
        . evalState emptyLocalTypeEnvironment
        . runWriter @(Constraint SourceRegion)
        . runReader emptyContextStack
        . inject

runInferSCCQuery ::
    SupportsQuery QueryRequiredDeclarationByName Shunted =>
    SCCKey ->
    Eff
        (ConsQueryEffects (Rock.Rock Query : r))
        (Map (Qualified VarName) (Polytype SourceRegion))
runInferSCCQuery key = fst <$> runInferEffects (evalState initialInferState $ inferSCC (sccKeyToSCC key))

seedSCC :: (SupportsQuery QueryRequiredDeclarationByName Shunted, _) => SCC (Qualified VarName) -> Eff r ()
seedSCC scc = do
    logDebug $ "Seeding SCC: " <> pretty (flattenSCC scc)
    for_ scc $ \component -> do
        decl <- runErrorOrReport @ShuntError $ Rock.fetch $ Elara.Query.RequiredDeclarationByName @Shunted (NVarName <$> component)
        seedDeclaration decl

inferSCC ::
    ( SupportsQuery QueryRequiredDeclarationByName Shunted
    , _
    ) =>
    SCC (Qualified VarName) -> Eff r (Map (Qualified VarName) (Polytype SourceRegion))
inferSCC scc = do
    prettyState <- pretty <$> get @(TypeEnvironment SourceRegion)
    logDebug $ "Seeding SCC complete. Environment:\n" <> prettyState
    inferred <- for scc $ \component -> do
        decl <- runErrorOrReport @ShuntError $ Rock.fetch $ Elara.Query.RequiredDeclarationByName @Shunted (NVarName <$> component)
        inferred <- inferDeclarationScheme decl
        pure (component, inferred)

    pure $ fromList @(Map _ _) (toList inferred)

runTypeCheckedExprQuery :: Qualified VarName -> Eff (ConsQueryEffects (Rock.Rock Query : r)) TypedExpr
runTypeCheckedExprQuery name = do
    mod <- Rock.fetch $ TypeCheckedModule (qualifier name)
    let NewModule.Module _ m' = mod
    case find (matchesValueName name) m'.moduleDeclarations of
        Just (New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ (New.ValueDeclaration _ e _ _ _ _)))) -> pure e
        _ -> error $ "could not find declaration for " <> show name
  where
    matchesValueName :: Qualified VarName -> New.Declaration SourceRegion Typed -> Bool
    matchesValueName qn (New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body'))) =
        case body' of
            New.ValueDeclaration n _ _ _ _ _ -> n ^. unlocated == qn
            _ -> False

runTypeCheckedDeclarationQuery :: Qualified Name -> Eff (ConsQueryEffects (Rock.Rock Query : r)) TypedDeclaration
runTypeCheckedDeclarationQuery name = do
    shuntedDecl <- runErrorOrReport @ShuntError $ Rock.fetch (Elara.Query.RequiredDeclarationByName @Shunted name)
    let New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ shuntedBody')) = shuntedDecl
    (typedDecl, _) <- runInferEffects $ evalState initialInferState $ case shuntedBody' of
        New.ValueDeclaration valueName expr _ _ _ _ -> do
            let varName = valueName ^. unlocated

            deps <- Rock.fetch (Elara.Query.FreeVarsOf varName)
            sccKey <- Rock.fetch (Elara.Query.SCCKeyOf varName)
            let scc = sccKeyToSCC sccKey
            let sccSet = Set.fromList (flattenSCC scc)

            for_ deps $ \dep -> do
                -- Only seed if it's not part of the current recursive cycle
                unless (dep `Set.member` sccSet) $ do
                    -- Fetch the type from Rock (cached)
                    t <- Rock.fetch (Elara.Query.TypeOf (TermVarKey dep))
                    -- Add to the local inference state
                    addType' (TermVarKey dep) t

            -- Values might use constructors,
            -- so we need to ensure their types are in the environment.
            let usedConstructors = collectConstructors expr

            for_ usedConstructors $ \ctorName -> do
                -- This helper fetches the module defining the Ctor and registers types
                seedConstructorsFor (qualifier ctorName)

            -- infer the entire SCC together to solve mutual recursion constraints.
            inferredDecls <- for scc $ \sccMemberName -> do
                -- Fetch member source
                memberDecl <- runErrorOrReport @ShuntError $ Rock.fetch (Elara.Query.RequiredDeclarationByName @Shunted (NVarName <$> sccMemberName))
                inferDeclaration memberDecl

            case find (\d -> declarationName d == (name ^. unqualified)) inferredDecls of
                Just d -> pure d
                Nothing -> error $ "Impossible: Declaration " <> show name <> " not found in its own SCC"
        New.TypeDeclarationBody{} -> do
            seedConstructorsFor (qualifier name)
            inferDeclaration shuntedDecl
        New.DeclBodyExtension v -> absurd v

    pure typedDecl

-- | Get the name from a typed declaration
declarationName :: New.Declaration SourceRegion Typed -> Name
declarationName (New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body'))) =
    case body' of
        New.ValueDeclaration n _ _ _ _ _ -> toName (n ^. unlocated)
        New.TypeDeclarationBody n _ _ _ _ _ -> toName (n ^. unlocated)
        New.DeclBodyExtension v -> absurd v

-- | Collect all constructor references from a shunted expression
collectConstructors :: NewS.ShuntedExpr -> [Qualified TypeName]
collectConstructors (New.Expr _ _ e') = case e' of
    New.ECon _ (Located _ qtn) -> [qtn]
    New.EVar _ _ -> []
    New.EInt _ -> []
    New.EFloat _ -> []
    New.EString _ -> []
    New.EChar _ -> []
    New.EUnit -> []
    New.ELam _ _ body -> collectConstructors body
    New.EApp _ f x -> collectConstructors f <> collectConstructors x
    New.ETyApp e _ -> collectConstructors e
    New.EIf c t f -> collectConstructors c <> collectConstructors t <> collectConstructors f
    New.EMatch e cases -> collectConstructors e <> concatMap (\(p, b) -> collectPatternConstructors p <> collectConstructors b) cases
    New.ELetIn _ _ e1 e2 -> collectConstructors e1 <> collectConstructors e2
    New.ELet _ _ e1 -> collectConstructors e1
    New.EBlock exprs -> concatMap collectConstructors (toList exprs)
    New.EAnn e _ -> collectConstructors e
    New.EExtension v -> absurd v

-- | Collect constructor references from a shunted pattern
collectPatternConstructors :: NewS.ShuntedPattern -> [Qualified TypeName]
collectPatternConstructors (New.Pattern _ _ p') = case p' of
    New.PCon (Located _ qtn) pats -> qtn : concatMap collectPatternConstructors pats
    _ -> []

inferModule ::
    forall r.
    (InferPipelineEffects r, Infer SourceRegion r) =>
    NewModule.Module SourceRegion Shunted ->
    Eff r (NewModule.Module SourceRegion Typed)
inferModule (NewModule.Module loc m') = do
    typedDecls <- traverse inferDeclaration m'.moduleDeclarations
    let typedExposing = phaseCoerce m'.moduleExposing :: NewModule.Exposing SourceRegion Typed
        typedImports = phaseCoerce m'.moduleImports :: [NewModule.Import SourceRegion Typed]
    pure $ NewModule.Module loc $ NewModule.Module' m'.moduleName typedExposing typedImports typedDecls

-- | Add's a declaration's name and expected type to the type environment
seedDeclaration ::
    _ =>
    New.Declaration SourceRegion Shunted -> Eff r ()
seedDeclaration (New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body'))) =
    case body' of
        New.ValueDeclaration valueName _ _ _ valueType _ -> logDebugWith ("seedDeclaration: Value " <> pretty valueName) $ do
            expectedType <- traverse (inferTypeKind >=> astTypeToGeneralisedInferType) valueType
            logDebug $ "Expected type for " <> pretty valueName <> ": " <> pretty expectedType
            expected <- case expectedType of
                Just t -> pure t
                Nothing -> Lifted . TypeVar (valueName ^. sourceRegion) . UnificationVar <$> makeUniqueTyVar
            -- When we have an expected type (e.g., from a user annotation), skolemise
            -- its quantified variables so they cannot unify with concrete types.
            expectedAsMono <- skolemise expected
            logDebug $ "Skolemised expected type of" <+> pretty valueName <+> ": " <> pretty expectedAsMono
            addType' (TermVarKey (valueName ^. unlocated)) expected
        _ -> pass -- TODO

inferDeclarationScheme :: _ => New.Declaration SourceRegion Shunted -> Eff r (Polytype SourceRegion)
inferDeclarationScheme (New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body'))) = case body' of
    New.ValueDeclaration valueName valueExpr _ _ _ _ -> logDebugWith ("inferDeclarationScheme: " <> pretty valueName) $ do
        expectedType <- lookupType (TermVarKey (valueName ^. unlocated))
        (_, polytype) <- inferValue (valueName ^. unlocated) valueExpr (Just expectedType)
        addType' (TermVarKey (valueName ^. unlocated)) (Polytype polytype)
        logDebug $ "Inferred type for " <> pretty valueName <> ": " <> pretty polytype
        pure polytype
    _ -> error "only value declarations are supported currently"

inferDeclaration ::
    forall r.
    (HasCallStack, InferPipelineEffects r, Infer SourceRegion r) =>
    New.Declaration SourceRegion Shunted ->
    Eff r (New.Declaration SourceRegion Typed)
inferDeclaration (New.Declaration dloc (New.Declaration' mn (New.DeclarationBody bloc body'))) = do
    typedBody' <- inferDeclarationBody' body'
    pure $ New.Declaration dloc (New.Declaration' mn (New.DeclarationBody bloc typedBody'))
  where
    inferDeclarationBody' ::
        HasCallStack =>
        New.DeclarationBody' SourceRegion Shunted ->
        Eff r (New.DeclarationBody' SourceRegion Typed)
    inferDeclarationBody' = \case
        New.ValueDeclaration name e () () _valueTypeMeta anns -> do
            expectedType <- traverse (inferTypeKind >=> astTypeToGeneralisedInferType) _valueTypeMeta
            logDebug $ "Expected type for " <> pretty name <> ": " <> pretty expectedType
            (typedExpr, polytype) <- inferValue (name ^. unlocated) e expectedType
            logDebug $ "Inferred type for " <> pretty name <> ": " <> pretty polytype
            addType' (TermVarKey (name ^. unlocated)) (Polytype polytype)
            typedAnns <- traverse inferAnnotation anns

            pure (New.ValueDeclaration name typedExpr () () (Polytype polytype) typedAnns)
        New.TypeDeclarationBody name tyVars bodyDecl _mKind NoExtension anns -> do
            (kind, decl') <- inferKind (name ^. unlocated) tyVars bodyDecl
            case decl' of
                New.Alias t -> do
                    _ <- astTypeToInferTypeWithKind t
                    let tyVars' = fmapToSnd createTypeVar tyVars

                    typedAnns <- traverse inferAnnotation anns

                    pure
                        ( New.TypeDeclarationBody
                            name
                            (zipWith (<$) (snd <$> tyVars') tyVars)
                            (kindedToTypedTypeDecl decl')
                            Nothing -- TODO: kind
                            kind
                            typedAnns
                        )
                New.ADT ctors -> do
                    let tyVars' = fmapToSnd createTypeVar tyVars
                    let typeConstructorType = TypeConstructor (name ^. sourceRegion) (name ^. unlocated) (fmap (\(tyVarLocation, tyVar) -> TypeVar (tyVarLocation ^. sourceRegion) $ UnificationVar tyVar) tyVars')

                    let inferCtor (ctorName, t :: [KindedType]) = do
                            t' <- traverse astTypeToInferTypeWithKind t
                            let ctorType =
                                    foldr (Function (ctorName ^. sourceRegion) . fst) typeConstructorType t'
                            addType' (DataConKey (ctorName ^. unlocated)) (Polytype (Forall (ctorName ^. sourceRegion) (snd <$> tyVars') (EmptyConstraint (monotypeLoc ctorType)) ctorType))

                            pure (ctorName, t')

                    traverse_ inferCtor ctors
                    typedAnns <- traverse inferAnnotation anns

                    pure
                        ( New.TypeDeclarationBody
                            name
                            (zipWith (<$) (snd <$> tyVars') tyVars)
                            (kindedToTypedTypeDecl decl')
                            Nothing -- TODO: kind
                            kind
                            typedAnns
                        )
        New.DeclBodyExtension v -> absurd v

createTypeVar :: Located (Unique LowerAlphaName) -> UniqueTyVar
createTypeVar (Located _ u) = fmap (Just . nameText) u

{- | Convert a kinded type declaration to a typed type declaration
The only difference is the TypeVariable: Kinded uses Located (Unique LowerAlphaName),
Typed uses Located UniqueTyVar. TypeMeta and ConstructorBinder are the same.
-}
kindedToTypedTypeDecl :: New.TypeDeclaration SourceRegion NewK.Kinded -> New.TypeDeclaration SourceRegion Typed
kindedToTypedTypeDecl (New.ADT ctors) = New.ADT (secondF (fmap kindedToTypedType) ctors)
kindedToTypedTypeDecl (New.Alias t) = New.Alias (kindedToTypedType t)

kindedToTypedType :: New.Type SourceRegion NewK.Kinded -> New.Type SourceRegion Typed
kindedToTypedType (New.Type loc meta t') = New.Type loc meta (kindedToTypedType' t')

kindedToTypedType' :: New.Type' SourceRegion NewK.Kinded -> New.Type' SourceRegion Typed
kindedToTypedType' = \case
    New.TVar v -> New.TVar ((Just . nameText) <<$>> v)
    New.TFun t1 t2 -> New.TFun (kindedToTypedType t1) (kindedToTypedType t2)
    New.TUnit -> New.TUnit
    New.TApp t1 t2 -> New.TApp (kindedToTypedType t1) (kindedToTypedType t2)
    New.TUserDefined n -> New.TUserDefined n
    New.TRecord fields -> New.TRecord (secondF kindedToTypedType fields)
    New.TList t -> New.TList (kindedToTypedType t)
    New.TExtension v -> absurd v

inferAnnotation :: _ => New.Annotation SourceRegion Shunted -> Eff r (New.Annotation SourceRegion Typed)
inferAnnotation (New.Annotation name args) = do
    args' <-
        traverse
            inferAnnotationArg
            args
    pure (New.Annotation name args')

inferAnnotationArg ::
    _ =>
    New.AnnotationArg SourceRegion Shunted ->
    Eff r (New.AnnotationArg SourceRegion Typed)
inferAnnotationArg (New.AnnotationArg e) = do
    -- We don't have an expected type for annotation arguments since they are unnamed
    ((typedExpr, t), constraint) <- runWriter $ generateConstraints e

    (finalConstraint, subst) <- solveConstraint mempty (fuv t <> fuv constraint) constraint
    case finalConstraint of
        EmptyConstraint _ -> pass
        _ -> throwError $ UnresolvedConstraint undefined finalConstraint

    pure $ New.AnnotationArg (getExpr (substituteAll subst (SubstitutableExpr typedExpr)))

inferValue ::
    forall r.
    (Error (UnifyError SourceRegion) :> r, _) =>
    Qualified VarName ->
    NewS.ShuntedExpr ->
    Maybe (Type SourceRegion) ->
    Eff r (TypedExpr, Polytype SourceRegion)
inferValue valueName valueExpr expectedType = do
    -- generate
    let exprLoc = exprLocation valueExpr
    expected <- case expectedType of
        Just t -> pure t
        Nothing -> Lifted . TypeVar exprLoc . UnificationVar <$> makeUniqueTyVar
    -- When we have an expected type (e.g., from a user annotation), skolemise
    -- its quantified variables so they cannot unify with concrete types.
    expectedAsMono <- skolemise expected
    addType' (TermVarKey valueName) expected
    ((typedExpr, t), constraint) <- runWriter $ generateConstraints valueExpr

    let constraint' = constraint <> simpleEquality (typeLoc expected) expectedAsMono t
    let tch = fuv t <> fuv constraint'
    logDebug $ "Generated constraints: " <> pretty constraint' <> " for " <> pretty valueName
    logDebug $ "Type: " <> pretty t

    (finalConstraint, subst) <- solveConstraint mempty tch constraint'

    case finalConstraint of
        EmptyConstraint _ -> pass
        _ -> throwError $ UnresolvedConstraint valueName finalConstraint

    let newType = substituteAll subst t

    logDebug $ "Substituted type: " <> pretty newType <> " from " <> pretty t <> " with " <> pretty subst

    generalized <- generalise (removeSkolems newType)

    logDebug $ "Generalized type: " <> pretty generalized <> " from " <> pretty newType

    pure (getExpr (substituteAll subst (SubstitutableExpr typedExpr)), generalized)

-- | Get the location of an expression
exprLocation :: New.Expr loc p -> loc
exprLocation (New.Expr loc _ _) = loc

-- Replace all quantified variables in a type scheme with rigid skolem variables.
-- This prevents ill-typed programs from unifying annotated polymorphic variables
-- with concrete types during checking.
skolemise :: forall r. Type SourceRegion -> Eff r (Monotype SourceRegion)
skolemise = \case
    Lifted t -> pure t
    Polytype (Forall loc tyVars _ t) -> do
        -- Build a substitution mapping each quantified variable α to a rigid skolem #α
        let pairs = zip (fmap (view typed) tyVars) (TypeVar loc . SkolemVar <$> tyVars)
            subst = Substitution $ fromList @(Map _ _) pairs
        pure $ substituteAll subst t

newtype SubstitutableExpr loc = SubstitutableExpr {getExpr :: TypedExpr} deriving (Show, Eq, Ord)

instance Substitutable SubstitutableExpr SourceRegion where
    substitute tv t (SubstitutableExpr expr) =
        let New.Expr loc meta e' = expr
            meta' = substitute tv t meta
            e'' = substituteExpr' tv t e'
         in SubstitutableExpr (New.Expr loc meta' e'')

    -- overridden default for performance (provides a > 300% speedup by avoiding repeated traversals over potentially large expressions)
    substituteAll s (SubstitutableExpr expr) =
        let New.Expr loc meta e' = expr
            meta' = substituteAll s meta
            e'' = substituteAllExpr' s e'
         in SubstitutableExpr (New.Expr loc meta' e'')

-- | Substitute a type variable in an expression body
substituteExpr' :: UniqueTyVar -> Monotype SourceRegion -> TypedExpr' -> TypedExpr'
substituteExpr' tv t = \case
    New.EInt i -> New.EInt i
    New.EFloat f -> New.EFloat f
    New.EString s -> New.EString s
    New.EChar c -> New.EChar c
    New.EUnit -> New.EUnit
    New.EVar vt v -> New.EVar (substitute tv t vt) v
    New.ECon ext v -> New.ECon ext v
    New.ELam ext binder body -> New.ELam ext (substituteLambdaBinder tv t binder) (substituteExpr tv t body)
    New.EApp ext f x -> New.EApp ext (substituteExpr tv t f) (substituteExpr tv t x)
    New.ETyApp e ty -> New.ETyApp (substituteExpr tv t e) (substituteAstType tv t ty)
    New.EIf c th el -> New.EIf (substituteExpr tv t c) (substituteExpr tv t th) (substituteExpr tv t el)
    New.EMatch e cases -> New.EMatch (substituteExpr tv t e) (bimapF (substitutePattern tv t) (substituteExpr tv t) cases)
    New.ELetIn ext binder e1 e2 -> New.ELetIn ext binder (substituteExpr tv t e1) (substituteExpr tv t e2)
    New.ELet ext binder e1 -> New.ELet ext binder (substituteExpr tv t e1)
    New.EBlock exprs -> New.EBlock (fmap (substituteExpr tv t) exprs)
    New.EAnn e ty -> New.EAnn (substituteExpr tv t e) (substituteAstType tv t ty)
    New.EExtension v -> absurd v

-- | Bulk substitute in expression body (performance optimized)
substituteAllExpr' :: Substitution SourceRegion -> TypedExpr' -> TypedExpr'
substituteAllExpr' s = \case
    New.EInt i -> New.EInt i
    New.EFloat f -> New.EFloat f
    New.EString s' -> New.EString s'
    New.EChar c -> New.EChar c
    New.EUnit -> New.EUnit
    New.EVar vt v -> New.EVar (substituteAll s vt) v
    New.ECon ext v -> New.ECon ext v
    New.ELam ext binder body -> New.ELam ext (substituteAllLambdaBinder s binder) (substituteAllExpr s body)
    New.EApp ext f x -> New.EApp ext (substituteAllExpr s f) (substituteAllExpr s x)
    New.ETyApp e ty -> New.ETyApp (substituteAllExpr s e) (substituteAllAstType s ty)
    New.EIf c th el -> New.EIf (substituteAllExpr s c) (substituteAllExpr s th) (substituteAllExpr s el)
    New.EMatch e cases -> New.EMatch (substituteAllExpr s e) (bimapF (substituteAllPattern s) (substituteAllExpr s) cases)
    New.ELetIn ext binder e1 e2 -> New.ELetIn ext binder (substituteAllExpr s e1) (substituteAllExpr s e2)
    New.ELet ext binder e1 -> New.ELet ext binder (substituteAllExpr s e1)
    New.EBlock exprs -> New.EBlock (fmap (substituteAllExpr s) exprs)
    New.EAnn e ty -> New.EAnn (substituteAllExpr s e) (substituteAllAstType s ty)
    New.EExtension v -> absurd v

substituteExpr :: UniqueTyVar -> Monotype SourceRegion -> TypedExpr -> TypedExpr
substituteExpr tv t (New.Expr loc meta e') = New.Expr loc (substitute tv t meta) (substituteExpr' tv t e')

substituteAllExpr :: Substitution SourceRegion -> TypedExpr -> TypedExpr
substituteAllExpr s (New.Expr loc meta e') = New.Expr loc (substituteAll s meta) (substituteAllExpr' s e')

substitutePattern :: UniqueTyVar -> Monotype SourceRegion -> NewT.TypedPattern -> NewT.TypedPattern
substitutePattern tv t (New.Pattern loc meta p') = New.Pattern loc (substitute tv t meta) (substitutePattern' tv t p')

substituteAllPattern :: Substitution SourceRegion -> NewT.TypedPattern -> NewT.TypedPattern
substituteAllPattern s (New.Pattern loc meta p') = New.Pattern loc (substituteAll s meta) (substituteAllPattern' s p')

substitutePattern' :: UniqueTyVar -> Monotype SourceRegion -> NewT.TypedPattern' -> NewT.TypedPattern'
substitutePattern' tv t = \case
    New.PVar v -> New.PVar v
    New.PCon c ps -> New.PCon c (fmap (substitutePattern tv t) ps)
    New.PWildcard -> New.PWildcard
    New.PInt i -> New.PInt i
    New.PFloat f -> New.PFloat f
    New.PString s -> New.PString s
    New.PChar c -> New.PChar c
    New.PUnit -> New.PUnit
    New.PExtension v -> absurd v

substituteAllPattern' :: Substitution SourceRegion -> NewT.TypedPattern' -> NewT.TypedPattern'
substituteAllPattern' s = \case
    New.PVar v -> New.PVar v
    New.PCon c ps -> New.PCon c (fmap (substituteAllPattern s) ps)
    New.PWildcard -> New.PWildcard
    New.PInt i -> New.PInt i
    New.PFloat f -> New.PFloat f
    New.PString s -> New.PString s
    New.PChar c -> New.PChar c
    New.PUnit -> New.PUnit
    New.PExtension v -> absurd v

substituteLambdaBinder :: UniqueTyVar -> Monotype SourceRegion -> TypedLambdaParam (Unique VarName) SourceRegion Typed -> TypedLambdaParam (Unique VarName) SourceRegion Typed
substituteLambdaBinder tv t (TypedLambdaParam v meta) = TypedLambdaParam v (substitute tv t meta)

substituteAllLambdaBinder :: Substitution SourceRegion -> TypedLambdaParam (Unique VarName) SourceRegion Typed -> TypedLambdaParam (Unique VarName) SourceRegion Typed
substituteAllLambdaBinder s (TypedLambdaParam v meta) = TypedLambdaParam v (substituteAll s meta)

-- | Substitute a type variable in an AST type (New.Type SourceRegion Typed)
substituteAstType :: UniqueTyVar -> Monotype SourceRegion -> NewT.TypedType -> NewT.TypedType
substituteAstType tv t (New.Type loc kind t') = case t' of
    New.TVar (Located _ tv')
        | tv == tv' -> monotypeToAstType loc kind t
    _ -> New.Type loc kind (substituteAstType' tv t t')

substituteAstType' :: UniqueTyVar -> Monotype SourceRegion -> NewT.TypedType' -> NewT.TypedType'
substituteAstType' tv t = \case
    New.TVar v -> New.TVar v -- not matching (matching case handled in substituteAstType)
    New.TFun a b -> New.TFun (substituteAstType tv t a) (substituteAstType tv t b)
    New.TUnit -> New.TUnit
    New.TApp a b -> New.TApp (substituteAstType tv t a) (substituteAstType tv t b)
    New.TUserDefined n -> New.TUserDefined n
    New.TRecord fields -> New.TRecord (substituteAstType tv t <<$>> fields)
    New.TList a -> New.TList (substituteAstType tv t a)
    New.TExtension v -> absurd v

-- | Bulk substitute in an AST type
substituteAllAstType :: Substitution SourceRegion -> NewT.TypedType -> NewT.TypedType
substituteAllAstType (Substitution s) ty = foldl' (\acc (tv, t) -> substituteAstType tv t acc) ty (Map.toList s)

-- | Convert a Monotype to an AST Type for the Typed phase
monotypeToAstType :: SourceRegion -> ElaraKind -> Monotype SourceRegion -> NewT.TypedType
monotypeToAstType loc kind = \case
    TypeVar _ tv -> New.Type loc kind (New.TVar (Located loc (typeVarToUniqueTyVar tv)))
    Function _ a b -> New.Type loc kind (New.TFun (monotypeToAstType loc kind a) (monotypeToAstType loc kind b))
    TypeConstructor _ qn args -> case args of
        [] -> New.Type loc kind (New.TUserDefined (Located loc qn))
        _ -> foldl' (\acc arg -> New.Type loc kind (New.TApp acc (monotypeToAstType loc kind arg))) (New.Type loc kind (New.TUserDefined (Located loc qn))) args

typeVarToUniqueTyVar :: TypeVariable -> UniqueTyVar
typeVarToUniqueTyVar (UnificationVar tv) = tv
typeVarToUniqueTyVar (SkolemVar tv) = tv
