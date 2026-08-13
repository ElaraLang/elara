{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeAbstractions #-}

module Elara.TypeInfer (
    runTypeOfQuery,
    runTypeCheckedExprQuery,
    runTypeCheckedDeclarationQuery,
    runKindOfQuery,
    runInferSCCQuery,
    runGetTypeAliasQuery,
)
where

import Data.Generics.Product (HasType (typed))
import Data.Graph (SCC, flattenSCC)
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Local
import Effectful.Writer.Static.Local (Writer, runWriter)
import Relude.Extra (fmapToSnd, secondF)

import Data.Set qualified as Set

import Elara.AST.Location
import Elara.AST.Module
import Elara.AST.Name (LowerAlphaName (..), ModuleName (..), Name (..), NameLike (nameText), Qualified (..), ToName (..), TypeName, VarName (..), unqualified)
import Elara.AST.Phase (NoExtension (..))
import Elara.AST.PhaseCoerce (PhaseCoerce (..))
import Elara.AST.Phases.Kinded (KindedType)
import Elara.AST.Phases.Shunted (Shunted)
import Elara.AST.Phases.Typed (Typed, TypedDeclaration, TypedExpr)
import Elara.AST.Region (SourceRegion, unlocated)
import Elara.Data.Kind (KindVar)
import Elara.Data.Kind.Infer (KindInferError, inferKind, inferTypeKind, initialInferState, lookupKindVarMaybe)
import Elara.Data.Pretty
import Elara.Data.Unique (Unique)
import Elara.Data.Unique.Effect
import Elara.Error (ElaraError (..), ElaraWarning (..), reportElaraWarning, runErrorAsElaraError)
import Elara.Logging (StructuredDebug, logDebug, logDebugWith)
import Elara.Query (Query (..), RunPhase (..))
import Elara.Query.Effects
import Elara.Rules.Generic
import Elara.SCC.Type (SCCKey, sccKeyToSCC)
import Elara.Shunt ()
import Elara.Shunt.Error (ShuntError, ShuntWarning)
import Elara.TypeInfer.ConstraintGeneration
import Elara.TypeInfer.Context (emptyContextStack)
import Elara.TypeInfer.Convert (TypeConvertError, astTypeToGeneralisedInferType, astTypeToInferType, astTypeToInferTypeWithKind, kindedToTypedType)
import Elara.TypeInfer.Environment (InferError, LocalTypeEnvironment, TypeEnvKey (..), TypeEnvironment, addType', emptyLocalTypeEnvironment, emptyTypeEnvironment)
import Elara.TypeInfer.Error (UnifyError (..))
import Elara.TypeInfer.Ftv (Fuv (..))
import Elara.TypeInfer.Generalise
import Elara.TypeInfer.Monad
import Elara.TypeInfer.Substitute
import Elara.TypeInfer.Type (Constraint (..), Monotype (..), Polytype (..), Substitutable (..), Substitution (..), Type (..), TypeVariable (..), monotypeLoc, simpleEquality, typeLoc)
import Elara.TypeInfer.Unique (UniqueTyVar, makeUniqueTyVar)

import Elara.AST.Phases.Kinded qualified as NewK
import Elara.AST.Phases.Shunted qualified as NewS
import Elara.AST.Types qualified as New
import Elara.Data.Kind.Infer qualified as Kind
import Rock qualified

instance PhaseCoerce (Exposing loc Shunted) (Exposing loc Typed)

instance PhaseCoerce (Exposition loc Shunted) (Exposition loc Typed)

instance PhaseCoerce (Import loc Shunted) (Import loc Typed)

instance PhaseCoerce (Import' loc Shunted) (Import' loc Typed)

instance PhaseCoerce (ImportExposingOrHiding loc Shunted) (ImportExposingOrHiding loc Typed)

type InferPipelineEffects r =
    ( StructuredDebug :> r
    , State Kind.InferState :> r
    , UniqueGen :> r
    , Error (UnifyError SourceRegion) :> r
    , Error TypeConvertError :> r
    , Error KindInferError :> r
    , Error ElaraError :> r
    , Infer SourceRegion r
    , Writer [ElaraWarning] :> r
    )

instance RunPhase Typed where
    getModuleByName mn = do
        (shunted, warnings) <- runWriter @(Set ShuntWarning) $ runErrorAsElaraError @ShuntError $ Rock.fetch $ Elara.Query.ModuleByName @Shunted mn
        traverse_ reportElaraWarning (Set.toList warnings)
        r <- runInferEffects $ evalState initialInferState (inferModule shunted)
        pure (fst r)

    getDeclarationByName = genericGetDeclarationByName @Typed getModuleByName
    getRequiredDeclarationByName = genericGetRequiredDeclarationByName @Typed getDeclarationByName
    getConstructorDeclaration = genericGetConstructorDeclaration @Typed getModuleByName
    getDeclarationAnnotations = genericGetDeclarationAnnotations @Typed getRequiredDeclarationByName
    getDeclarationAnnotationsOfType = genericGetDeclarationAnnotationsOfType @Typed getDeclarationAnnotations getConstructorDeclaration

-- | Run the 'TypeOf' query to get the type of a term or data constructor
runTypeOfQuery ::
    forall loc.
    loc ~ SourceRegion =>
    TypeEnvKey loc ->
    Eff
        ( ConsQueryEffects
            '[Rock.Rock Elara.Query.Query, Error ElaraError, Writer [ElaraWarning]]
        )
        (Type loc)
runTypeOfQuery key = fmap fst $ runInferEffects $ evalState initialInferState $ case key of
    TermVarKey varName -> logDebugWith ("TypeOf: " <> pretty varName) $ do
        sccs <- Rock.fetch $ GetSCCsOf varName
        logDebug $ "SCCs for " <> pretty varName <> ": " <> pretty (fmap flattenSCC sccs)
        -- Infer dependencies first to populate the environment
        for_ sccs seedSCC
        for_ sccs inferSCC
        -- Read from the now populated environment  without re-querying
        lookupType (TermVarKey varName)
    DataConKey con -> do
        seedConstructorsFor (qualifier con)
        lookupType (DataConKey con)

runKindOfQuery :: Qualified TypeName -> Eff (ConsQueryEffects '[Rock.Rock Query, Error ElaraError, Writer [ElaraWarning]]) (Maybe KindVar)
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
    Qualified TypeName ->
    Eff (ConsQueryEffects (Rock.Rock Query : Error ElaraError : Writer [ElaraWarning] : r)) (Maybe ([UniqueTyVar], Type SourceRegion))
runGetTypeAliasQuery name = do
    let modName = qualifier name
    (mod, warnings) <- runWriter @(Set ShuntWarning) $ runErrorAsElaraError @ShuntError $ Rock.fetch $ Elara.Query.ModuleByName @Shunted modName
    traverse_ reportElaraWarning (Set.toList warnings)

    let Module _ m' = mod
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

seedConstructorsFor :: (HasCallStack, QueryEffects r, Error ElaraError :> r, Writer [ElaraWarning] :> r, Rock.Rock Query :> r, StructuredDebug :> r, State Kind.InferState :> r, State (TypeEnvironment SourceRegion) :> r, State (LocalTypeEnvironment SourceRegion) :> r, Error KindInferError :> r, Error TypeConvertError :> r, Error (UnifyError SourceRegion) :> r) => ModuleName -> Eff r ()
seedConstructorsFor moduleName = logDebugWith ("seedConstructorsFor: " <> pretty moduleName) $ do
    -- Fetch all declarations in the module
    (mod, warnings) <- runWriter @(Set ShuntWarning) $ runErrorAsElaraError @ShuntError $ Rock.fetch $ Elara.Query.ModuleByName @Shunted moduleName
    traverse_ reportElaraWarning (Set.toList warnings)
    let Module _ m' = mod

    -- Extract type declarations
    let typeDecls = mapMaybe extractTypeDecl m'.moduleDeclarations

    for_ typeDecls $ \(name, _, _, _) -> Kind.preRegisterType (name ^. unlocated)

    -- For each type declaration, add its constructors to the environment
    for_ typeDecls $ \(name, typeVars, declBody, _anns) -> logDebugWith ("Seeding declaration: " <> pretty name) $ do
        (_, decl') <- inferKind (name ^. unlocated) typeVars declBody
        case decl' of
            New.Alias t -> do
                logDebug $ "Seeding alias type: " <> pretty name
                _ <- astTypeToInferType t
                pass -- we don't need to do anything with an alias i think
            New.ADT ctors -> do
                let tyVars' = fmap createTypeVar typeVars
                let nameLoc = unwrapLoc (getLocation name)
                let typeConstructorType =
                        TypeConstructor
                            nameLoc
                            (name ^. unlocated)
                            (fmap ((\tv -> TypeVar nameLoc tv []) . UnificationVar) tyVars')

                let inferCtor (ctorName, t :: [KindedType]) = do
                        t' <- traverse astTypeToInferTypeWithKind t
                        let ctorType =
                                foldr (Function nameLoc . fst) typeConstructorType t'
                        addType' (DataConKey (ctorName ^. unlocated)) (Polytype (Forall nameLoc tyVars' (EmptyConstraint (monotypeLoc ctorType)) ctorType))

                        pure (ctorName, t')

                for_ ctors inferCtor
  where
    extractTypeDecl :: New.Declaration SourceRegion Shunted -> Maybe (TaggedLocate TypeNode SourceRegion (Qualified TypeName), [TaggedLocate TypeNode SourceRegion (Unique LowerAlphaName)], New.TypeDeclaration SourceRegion Shunted, [New.Annotation SourceRegion Shunted])
    extractTypeDecl (New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body'))) =
        case body' of
            New.TypeDeclarationBody name typeVars declBody _ _ anns -> Just (name, typeVars, declBody, anns)
            _ -> Nothing

runInferEffects ::
    forall r a loc.
    (Pretty loc, QueryEffects r, Rock.Rock Query :> r, Eq loc, loc ~ SourceRegion, Error ElaraError :> r, Writer [ElaraWarning] :> r, Exception TypeConvertError) =>
    Eff
        ( InferEffectsCons
            loc
            ( Error (UnifyError loc)
                ': Error TypeConvertError
                ': Error KindInferError
                ': r
            )
        )
        a ->
    Eff r (a, Constraint loc)
runInferEffects =
    runErrorAsElaraError @(InferError _)
        . runErrorAsElaraError @(UnifyError _)
        . runErrorAsElaraError @KindInferError
        . runErrorAsElaraError @TypeConvertError
        . evalState emptyTypeEnvironment
        . evalState emptyLocalTypeEnvironment
        . runWriter @(Constraint SourceRegion)
        . runReader emptyContextStack
        . inject

runInferSCCQuery ::
    SCCKey ->
    Eff
        (ConsQueryEffects '[Rock.Rock Query, Error ElaraError, Writer [ElaraWarning]])
        (Map (Qualified VarName) (Polytype SourceRegion))
runInferSCCQuery key = fst <$> runInferEffects (evalState initialInferState $ inferSCC (sccKeyToSCC key))

seedSCC :: (QueryEffects r, Error ElaraError :> r, Writer [ElaraWarning] :> r, Rock.Rock Query :> r, StructuredDebug :> r, Error (UnifyError SourceRegion) :> r, Error KindInferError :> r, Error TypeConvertError :> r, State Kind.InferState :> r, State (TypeEnvironment SourceRegion) :> r, State (LocalTypeEnvironment SourceRegion) :> r, Infer SourceRegion r) => SCC (Qualified VarName) -> Eff r ()
seedSCC scc = do
    logDebug $ "Seeding SCC: " <> pretty (flattenSCC scc)
    for_ scc $ \component -> do
        (decl :: New.Declaration SourceRegion Shunted, warnings) <-
            runWriter @(Set ShuntWarning) $
                runErrorAsElaraError @ShuntError $
                    Rock.fetch $
                        Elara.Query.RequiredDeclarationByName @Shunted (toName <$> component)
        traverse_ reportElaraWarning (Set.toList warnings)
        seedDeclaration decl

inferSCC ::
    (InferPipelineEffects r, Infer SourceRegion r) =>
    SCC (Qualified VarName) -> Eff r (Map (Qualified VarName) (Polytype SourceRegion))
inferSCC scc = do
    prettyState <- pretty <$> get @(TypeEnvironment SourceRegion)
    logDebug $ "Seeding SCC complete. Environment:\n" <> prettyState
    inferred <- for scc $ \component -> do
        (decl, warnings) <-
            runWriter @(Set ShuntWarning) $
                runErrorAsElaraError @ShuntError $
                    Rock.fetch $
                        Elara.Query.RequiredDeclarationByName @Shunted (toName <$> component)
        traverse_ reportElaraWarning (Set.toList warnings)
        inferred <- inferDeclarationScheme decl
        pure (component, inferred)

    pure $ fromList @(Map _ _) (toList inferred)

runTypeCheckedExprQuery :: Qualified VarName -> Eff (ConsQueryEffects (Rock.Rock Query : r)) TypedExpr
runTypeCheckedExprQuery name = do
    mod <- Rock.fetch $ Elara.Query.ModuleByName @Typed (qualifier name)
    let Module _ m' = mod
    case find (matchesValueName name) m'.moduleDeclarations of
        Just (New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ (New.ValueDeclaration _ e _ _ _ _)))) -> pure e
        _ -> error $ "could not find declaration for " <> show name
  where
    matchesValueName :: Qualified VarName -> New.Declaration SourceRegion Typed -> Bool
    matchesValueName qn (New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body'))) =
        case body' of
            New.ValueDeclaration n _ _ _ _ _ -> n ^. unlocated == qn
            _ -> False

runTypeCheckedDeclarationQuery :: Qualified Name -> Eff (ConsQueryEffects '[Rock.Rock Elara.Query.Query, Error ElaraError, Writer [ElaraWarning]]) TypedDeclaration
runTypeCheckedDeclarationQuery name = do
    let q = Elara.Query.RequiredDeclarationByName @Shunted
    (shuntedDecl, shuntedWarnings) <- runWriter @(Set ShuntWarning) $ runErrorAsElaraError @ShuntError $ Rock.fetch (q name)
    traverse_ reportElaraWarning (Set.toList shuntedWarnings)
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
                    t <- Rock.fetch (Elara.Query.TypeOf (TermVarKey dep))
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
                (memberDecl, warnings) <-
                    runWriter @(Set ShuntWarning) $
                        runErrorAsElaraError @ShuntError $
                            Rock.fetch (Elara.Query.RequiredDeclarationByName @Shunted (toName <$> sccMemberName))
                traverse_ reportElaraWarning (Set.toList warnings)
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
    New.ECon _ (TaggedLocate _ qtn) -> [qtn]
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
    New.PCon (TaggedLocate _ qtn) pats -> qtn : concatMap collectPatternConstructors pats
    _ -> []

inferModule ::
    forall r.
    (InferPipelineEffects r, Infer SourceRegion r) =>
    Module SourceRegion Shunted ->
    Eff r (Module SourceRegion Typed)
inferModule (Module loc m') = do
    typedDecls <- traverse inferDeclaration m'.moduleDeclarations
    let typedExposing = phaseCoerce m'.moduleExposing :: Exposing SourceRegion Typed
        typedImports = phaseCoerce m'.moduleImports :: [Import SourceRegion Typed]
    pure $ Module loc $ Module' m'.moduleName typedExposing typedImports typedDecls

-- | Add's a declaration's name and expected type to the type environment
seedDeclaration ::
    forall r.
    (HasCallStack, InferPipelineEffects r, Infer SourceRegion r) =>
    New.Declaration SourceRegion Shunted -> Eff r ()
seedDeclaration (New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body'))) =
    case body' of
        New.ValueDeclaration valueName _ _ _ valueType _ -> logDebugWith ("seedDeclaration: Value " <> pretty valueName) $ do
            expectedType <- traverse (inferTypeKind >=> astTypeToGeneralisedInferType) valueType
            logDebug $ "Expected type for " <> pretty valueName <> ": " <> pretty expectedType
            expected <- case expectedType of
                Just t -> pure t
                Nothing -> Lifted . (\tv -> TypeVar (getLocation $ stripTag valueName) tv []) . UnificationVar <$> makeUniqueTyVar
            -- When we have an expected type (e.g. from a user annotation), skolemise
            -- its quantified variables so they cannot unify with concrete types.
            expectedAsMono <- skolemise expected
            logDebug $ "Skolemised expected type of" <+> pretty valueName <+> ": " <> pretty expectedAsMono
            addType' (TermVarKey (valueName ^. unlocated)) expected
        _ -> pass -- TODO

inferDeclarationScheme :: forall r. (InferPipelineEffects r, Infer SourceRegion r) => New.Declaration SourceRegion Shunted -> Eff r (Polytype SourceRegion)
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
                    let typeConstructorType =
                            TypeConstructor
                                (getLocation $ stripTag name)
                                (name ^. unlocated)
                                (fmap (\(tyVarLocation, tyVar) -> TypeVar (getLocation $ stripTag tyVarLocation) (SkolemVar tyVar) []) tyVars')

                    let inferCtor (ctorName, t :: [KindedType]) = do
                            t' <- traverse astTypeToInferTypeWithKind t
                            let ctorType =
                                    foldr (Function (getLocation $ stripTag ctorName) . fst) typeConstructorType t'
                            addType' (DataConKey (stripTag ctorName ^. unlocated)) (Polytype (Forall (getLocation $ stripTag ctorName) (snd <$> tyVars') (EmptyConstraint (monotypeLoc ctorType)) ctorType))

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

createTypeVar :: TaggedLocate TypeNode SourceRegion (Unique LowerAlphaName) -> UniqueTyVar
createTypeVar (TaggedLocate _ u) = fmap (Just . nameText) u

-- | Convert a kinded type declaration to a typed type declaration
kindedToTypedTypeDecl :: New.TypeDeclaration SourceRegion NewK.Kinded -> New.TypeDeclaration SourceRegion Typed
kindedToTypedTypeDecl (New.ADT ctors) = New.ADT (secondF (fmap kindedToTypedType) ctors)
kindedToTypedTypeDecl (New.Alias t) = New.Alias (kindedToTypedType t)

inferAnnotation :: forall r. (InferPipelineEffects r, Infer SourceRegion r) => New.Annotation SourceRegion Shunted -> Eff r (New.Annotation SourceRegion Typed)
inferAnnotation (New.Annotation name args) = do
    args' <-
        traverse
            inferAnnotationArg
            args
    pure (New.Annotation name args')

inferAnnotationArg ::
    forall r.
    (InferPipelineEffects r, Infer SourceRegion r) =>
    New.AnnotationArg SourceRegion Shunted ->
    Eff r (New.AnnotationArg SourceRegion Typed)
inferAnnotationArg (New.AnnotationArg e) = do
    -- We don't have an expected type for annotation arguments since they are unnamed
    ((typedExpr, t), constraint) <- runWriter $ generateConstraints e

    (finalConstraint, subst) <- solveConstraint mempty (fuv t <> fuv constraint) constraint
    case finalConstraint of
        EmptyConstraint _ -> pass
        _ ->
            let fallbackName = Qualified "annotation" (ModuleName (pure "<annotation>"))
             in throwError $ UnresolvedConstraint fallbackName finalConstraint

    pure $ New.AnnotationArg (getExpr (substituteAll subst (SubstitutableExpr typedExpr)))

inferValue ::
    forall r.
    (HasCallStack, InferPipelineEffects r, Infer SourceRegion r) =>
    Qualified VarName ->
    NewS.ShuntedExpr ->
    Maybe (Type SourceRegion) ->
    Eff r (TypedExpr, Polytype SourceRegion)
inferValue valueName valueExpr expectedType = do
    -- generate
    let exprLoc = exprLocation valueExpr
    expected <- case expectedType of
        Just t -> pure t
        Nothing -> Lifted . (\tv -> TypeVar (getLocation exprLoc) tv []) . UnificationVar <$> makeUniqueTyVar
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

    (generalised, genSubst) <- generalise (removeSkolems newType)

    let finalSubst = subst <> genSubst

    logDebug $ "Generalised type: " <> pretty generalised <> " from " <> pretty newType

    pure (getExpr (substituteAll finalSubst (SubstitutableExpr typedExpr)), generalised)

-- | Get the location of an expression
exprLocation :: New.Expr loc p -> NodeLoc ExprNode loc
exprLocation (New.Expr loc _ _) = loc

-- Replace all quantified variables in a type scheme with rigid skolem variables.
-- This prevents ill-typed programs from unifying annotated polymorphic variables
-- with concrete types during checking.
skolemise :: forall r. Type SourceRegion -> Eff r (Monotype SourceRegion)
skolemise = \case
    Lifted t -> pure t
    Polytype (Forall loc tyVars _ t) -> do
        -- Build a substitution mapping each quantified variable α to a rigid skolem #α
        let pairs = zip (fmap (view typed) tyVars) ((\tv -> TypeVar loc tv []) . SkolemVar <$> tyVars)
            subst = Substitution $ fromList @(Map _ _) pairs
        pure $ substituteAll subst t
