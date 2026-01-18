{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module Elara.TypeInfer where

import Data.Generics.Product
import Data.Generics.Sum (AsAny (_As))
import Data.Generics.Wrapped (_Unwrapped)
import Data.Graph (SCC, flattenSCC)
import Data.Set qualified as Set
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Effectful.Writer.Static.Local (runWriter)
import Elara.AST.Generic (
    Declaration (Declaration),
    Declaration' (Declaration'),
    DeclarationBody (DeclarationBody),
 )
import Elara.AST.Generic qualified as Generic
import Elara.AST.Generic.Common
import Elara.AST.Generic.Types (
    DeclarationBody' (..),
 )
import Elara.AST.Kinded
import Elara.AST.Module
import Elara.AST.Name (LowerAlphaName, ModuleName, Name (..), NameLike (nameText), Qualified (..), TypeName, VarName, unqualified)
import Elara.AST.Region (HasSourceRegion (..), Located (Located), SourceRegion, unlocated, withLocationOf)
import Elara.AST.Select (
    LocatedAST (
        Shunted,
        Typed
    ),
 )
import Elara.AST.Shunted as Shunted
import Elara.AST.Typed as Typed
import Elara.Data.Kind (KindVar)
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
import Elara.TypeInfer.Convert (TypeConvertError, astTypeToGeneralisedInferType, astTypeToInferType, astTypeToInferTypeWithKind)
import Elara.TypeInfer.Environment (InferError, TypeEnvKey (..), TypeEnvironment, addType', emptyLocalTypeEnvironment, emptyTypeEnvironment)
import Elara.TypeInfer.Ftv (Fuv (..))
import Elara.TypeInfer.Generalise
import Elara.TypeInfer.Monad
import Elara.TypeInfer.Type (Constraint (..), Monotype (..), Polytype (..), Substitutable (..), Substitution (..), Type (..), TypeVariable (..), monotypeLoc, typeLoc)
import Elara.TypeInfer.Unique (UniqueTyVar, makeUniqueTyVar)
import Optics (forOf_)
import Relude.Extra (fmapToSnd)
import Rock qualified

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
        (Module Typed)
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

    let declarations = mod ^.. _Unwrapped % unlocated % field' @"declarations" % each
    let targetTypeName = name ^. unqualified
    let found = find (\d -> d ^? Generic.declarationName % unlocated % _As @"NTypeName" == Just targetTypeName) declarations

    case found of
        Just (Declaration decl) -> do
            res <- runInferEffects $ evalState initialInferState $ do
                let decl' = decl ^. unlocated

                case decl' ^. field' @"body" % _Unwrapped % unlocated of
                    TypeDeclaration _ typeVars (Located _ (Generic.Alias body)) _ -> do
                        -- infer the kind of the alias
                        (_, typedDeclBody) <- inferKind name typeVars (Generic.Alias body)

                        case typedDeclBody of
                            Generic.Alias typedBody -> do
                                inferBody <- astTypeToInferType typedBody

                                let uVars = fmap createTypeVar typeVars

                                pure (Just (uVars, Lifted inferBody))
                            _ -> pure Nothing
                    _ -> pure Nothing

            -- Extract the result from the inference pipeline
            pure (fst res)
        Nothing -> pure Nothing

seedConstructorsFor :: (SupportsQuery QueryModuleByName Shunted, HasCallStack, _) => ModuleName -> Eff r ()
seedConstructorsFor moduleName = debugWith ("seedConstructorsFor: " <> pretty moduleName) $ do
    -- Fetch all declarations in the module
    mod <- runErrorOrReport @ShuntError $ Rock.fetch $ Elara.Query.ModuleByName @Shunted moduleName
    let declarations =
            mod
                ^.. _Unwrapped
                % unlocated
                % field' @"declarations"
                % each
                % _Unwrapped
                % unlocated
                % field' @"body"
                % _Unwrapped
                % unlocated
                % _Ctor' @"TypeDeclaration"

    for_ declarations $ \(name, _, _, _) -> do
        Kind.preRegisterType (name ^. unlocated)

    -- For each type declaration, add its constructors to the environment
    for_ declarations $ \(name, typeVars, declBody, _) -> debugWith ("Seeding declaration: " <> pretty name) $ do
        (_, decl') <- inferKind (name ^. unlocated) typeVars (declBody ^. unlocated)
        case decl' of
            Generic.Alias t -> do
                logDebug $ "Seeding alias type: " <> pretty name
                _ <- astTypeToInferType t
                pass -- we don't need to do anything with an alias i think
            Generic.ADT ctors -> do
                let tyVars' = fmap createTypeVar typeVars
                let typeConstructorType = TypeConstructor (name ^. sourceRegion) (name ^. unlocated) (fmap (TypeVar (name ^. sourceRegion) . UnificationVar) tyVars')

                let inferCtor (ctorName, t :: [KindedType]) = do
                        t' <- traverse astTypeToInferTypeWithKind t
                        let ctorType =
                                foldr (Function (name ^. sourceRegion) . fst) typeConstructorType t'
                        addType' (DataConKey (ctorName ^. unlocated)) (Polytype (Forall (name ^. sourceRegion) tyVars' (EmptyConstraint (monotypeLoc ctorType)) ctorType))

                        pure (ctorName, t')

                for_ ctors inferCtor

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
        . inject

runInferSCCQuery ::
    SupportsQuery QueryRequiredDeclarationByName Shunted =>
    SCCKey ->
    Eff
        (ConsQueryEffects (Rock.Rock Query : r))
        (Map (Qualified VarName) (Polytype SourceRegion))
runInferSCCQuery key = do
    fst <$> runInferEffects (evalState initialInferState $ inferSCC (sccKeyToSCC key))

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
    debug $ "Seeding SCC complete. Environment:\n" <> prettyState
    inferred <- for scc $ \component -> do
        decl <- runErrorOrReport @ShuntError $ Rock.fetch $ Elara.Query.RequiredDeclarationByName @Shunted (NVarName <$> component)
        inferred <- inferDeclarationScheme decl
        pure (component, inferred)

    pure $ fromList @(Map _ _) (toList inferred)

runTypeCheckedExprQuery :: Qualified VarName -> Eff (ConsQueryEffects (Rock.Rock Query : r)) TypedExpr
runTypeCheckedExprQuery name = debugWithResult ("runTypeCheckedExprQuery: " <> pretty name) $ do
    mod <- Rock.fetch $ TypeCheckedModule (qualifier name)
    let decls = mod ^. _Unwrapped % unlocated % field' @"declarations"
    -- debug $ "Declarations in module:" <+> pretty decls
    case find
        (\(Declaration ld) -> (ld ^. unlocated % field' @"body" % _Unwrapped % unlocated) ^? _Ctor' @"Value" % _1 % unlocated == Just name)
        decls of
        Just (Declaration ld) -> case ld ^. unlocated % field' @"body" % _Unwrapped % unlocated of
            Value _ e _ _ _ -> pure e
            _ -> error "expected value declaration"
        Nothing -> error $ "could not find declaration for " <> show name

runTypeCheckedDeclarationQuery :: Qualified Name -> Eff (ConsQueryEffects (Rock.Rock Query : r)) TypedDeclaration
runTypeCheckedDeclarationQuery name = do
    shuntedDecl <- runErrorOrReport @ShuntError $ Rock.fetch (Elara.Query.RequiredDeclarationByName @Shunted name)
    (typedDecl, _) <- runInferEffects $ evalState initialInferState $ do
        case shuntedDecl ^. _Unwrapped % unlocated % field' @"body" % _Unwrapped % unlocated of
            Value valueName expr _ _ _ -> do
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
                -- so wee need to ensure their types are in the environment.
                let usedConstructors =
                        expr
                            ^.. cosmosOf gplate
                            % _Unwrapped
                            % _1
                            % unlocated
                            % _Ctor' @"Constructor"
                            % unlocated

                for_ usedConstructors $ \ctorName -> do
                    -- This helper fetches the module defining the Ctor and registers types
                    seedConstructorsFor (qualifier ctorName)

                -- infer the entire SCC together to solve mutual recursion constraints.
                inferredDecls <- for scc $ \sccMemberName -> do
                    -- Fetch member source
                    memberDecl <- runErrorOrReport @ShuntError $ Rock.fetch (Elara.Query.RequiredDeclarationByName @Shunted (NVarName <$> sccMemberName))
                    inferDeclaration memberDecl

                case find (\d -> (d ^. Generic.declarationName % unlocated) == (name ^. unqualified)) inferredDecls of
                    Just d -> pure d
                    Nothing -> error $ "Impossible: Declaration " <> show name <> " not found in its own SCC"
            TypeDeclaration{} -> do
                seedConstructorsFor (qualifier name)

                inferDeclaration shuntedDecl

    pure typedDecl

inferModule ::
    forall r.
    (InferPipelineEffects r, Infer SourceRegion r) =>
    Module Shunted ->
    Eff r (Module Typed)
inferModule m = do
    traverseModule inferDeclaration m

-- | Add's a declaration's name and expected type to the type environment
seedDeclaration ::
    _ =>
    ShuntedDeclaration -> Eff r ()
seedDeclaration (Declaration ld) =
    forOf_
        unlocated
        ld
        $ \d' -> case d' ^. field' @"body" % _Unwrapped % unlocated of
            Value valueName _ NoFieldValue valueType _ -> debugWith ("seedDeclaration: Value " <> pretty valueName) $ do
                expectedType <- traverse (inferTypeKind >=> astTypeToGeneralisedInferType) valueType
                debug $ "Expected type for " <> pretty valueName <> ": " <> pretty expectedType
                expected <- case expectedType of
                    Just t -> pure t
                    Nothing -> Lifted . TypeVar (valueName ^. sourceRegion) . UnificationVar <$> makeUniqueTyVar
                -- When we have an expected type (e.g., from a user annotation), skolemise
                -- its quantified variables so they cannot unify with concrete types.
                expectedAsMono <- skolemise expected
                debug $ "Skolemised expected type of" <+> pretty valueName <+> ": " <> pretty expectedAsMono
                addType' (TermVarKey (valueName ^. unlocated)) expected
            _ -> pass -- TODO

inferDeclarationScheme :: _ => ShuntedDeclaration -> Eff r (Polytype SourceRegion)
inferDeclarationScheme (view (_Unwrapped % unlocated % field' @"body" % _Unwrapped % unlocated) -> d) = case d of
    Value valueName valueExpr NoFieldValue _ _ -> debugWith ("inferDeclarationScheme: " <> pretty valueName) $ do
        expectedType <- lookupType (TermVarKey (valueName ^. unlocated))
        (_, polytype) <- inferValue (valueName ^. unlocated) valueExpr (Just expectedType)
        addType' (TermVarKey (valueName ^. unlocated)) (Polytype polytype)
        debug $ "Inferred type for " <> pretty valueName <> ": " <> pretty polytype
        pure polytype
    _ -> error "only value declarations are supported currently"

inferDeclaration ::
    forall r.
    (HasCallStack, InferPipelineEffects r, Infer SourceRegion r) =>
    ShuntedDeclaration ->
    Eff r TypedDeclaration
inferDeclaration (Declaration ld) = do
    Declaration
        <$> traverseOf
            unlocated
            ( \d' -> do
                let (DeclarationBody ldb) = d' ^. field' @"body"
                db' <-
                    DeclarationBody
                        <$> traverseOf
                            unlocated
                            inferDeclarationBody'
                            ldb
                pure (Declaration' (d' ^. field' @"moduleName") db')
            )
            ld
  where
    inferDeclarationBody' ::
        HasCallStack =>
        ShuntedDeclarationBody' ->
        Eff r TypedDeclarationBody'
    inferDeclarationBody' declBody = case declBody of
        Value name e NoFieldValue valueType annotations -> do
            expectedType <- traverse (inferTypeKind >=> astTypeToGeneralisedInferType) valueType
            debug $ "Expected type for " <> pretty name <> ": " <> pretty expectedType
            (typedExpr, polytype) <- inferValue (name ^. unlocated) e expectedType
            debug $ "Inferred type for " <> pretty name <> ": " <> pretty polytype
            addType' (TermVarKey (name ^. unlocated)) (Polytype polytype)
            annotations <- Generic.traverseValueDeclAnnotations inferAnnotation annotations
            pure (Value name typedExpr NoFieldValue (Polytype polytype) annotations)
        TypeDeclaration name tyVars body anns -> do
            (kind, decl') <- inferKind (name ^. unlocated) tyVars (body ^. unlocated)
            case decl' of
                Generic.Alias t -> do
                    typedBody <- astTypeToInferTypeWithKind t
                    let tyVars' = fmapToSnd createTypeVar tyVars

                    ann' <- case anns of
                        Generic.TypeDeclAnnotations kind_ anns' -> do
                            anns <- traverse inferAnnotation anns'
                            pure
                                Generic.TypeDeclAnnotations
                                    { kindAnn = kind
                                    , typeDeclAnnotations = anns
                                    }

                    pure
                        ( TypeDeclaration
                            name
                            (zipWith (<$) (snd <$> tyVars') tyVars)
                            (Generic.Alias (over _1 Lifted typedBody) `withLocationOf` body)
                            ann'
                        )
                Generic.ADT ctors -> do
                    let tyVars' = fmapToSnd createTypeVar tyVars
                    let typeConstructorType = TypeConstructor (name ^. sourceRegion) (name ^. unlocated) (fmap (\(tyVarLocation, tyVar) -> TypeVar (tyVarLocation ^. sourceRegion) $ UnificationVar tyVar) tyVars')

                    let inferCtor (ctorName, t :: [KindedType]) = do
                            t' <- traverse astTypeToInferTypeWithKind t
                            let ctorType =
                                    foldr (Function (ctorName ^. sourceRegion) . fst) typeConstructorType t'
                            addType' (DataConKey (ctorName ^. unlocated)) (Polytype (Forall (ctorName ^. sourceRegion) (snd <$> tyVars') (EmptyConstraint (monotypeLoc ctorType)) ctorType))

                            pure (ctorName, t')

                    ctors' <- traverse inferCtor ctors
                    ann' <- case anns of
                        Generic.TypeDeclAnnotations kind_ anns' -> do
                            anns <- traverse inferAnnotation anns'
                            pure
                                Generic.TypeDeclAnnotations
                                    { kindAnn = kind
                                    , typeDeclAnnotations = anns
                                    }

                    pure
                        ( TypeDeclaration
                            name
                            (zipWith (<$) (snd <$> tyVars') tyVars)
                            (Generic.ADT ctors' `withLocationOf` body)
                            ann'
                        )

createTypeVar :: Located (Unique LowerAlphaName) -> UniqueTyVar
createTypeVar (Located _ u) = fmap (Just . nameText) u

inferAnnotation :: _ => Generic.Annotation Shunted -> Eff r (Generic.Annotation Typed)
inferAnnotation (Generic.Annotation name args) = do
    args' <-
        traverse
            inferAnnotationArg
            args
    pure (Generic.Annotation name args')

inferAnnotationArg ::
    _ =>
    Generic.AnnotationArg Shunted ->
    Eff r (Generic.AnnotationArg Typed)
inferAnnotationArg (Generic.AnnotationArg e) = do
    -- We don't have an expected type for annotation arguments since they are unnamed
    ((typedExpr, t), constraint) <- runWriter $ generateConstraints e

    (finalConstraint, subst) <- solveConstraint mempty (fuv t <> fuv constraint) constraint
    case finalConstraint of
        EmptyConstraint _ -> pass
        _ -> throwError $ UnresolvedConstraint undefined finalConstraint

    pure $ Generic.AnnotationArg (getExpr (substituteAll subst (SubstitutableExpr typedExpr)))

inferValue ::
    forall r.
    (Error (UnifyError SourceRegion) :> r, _) =>
    Qualified VarName ->
    ShuntedExpr ->
    Maybe (Type SourceRegion) ->
    Eff r (TypedExpr, Polytype SourceRegion)
inferValue valueName valueExpr expectedType = do
    -- generate
    let exprLoc = valueExpr ^. Generic.exprLocation
    expected <- case expectedType of
        Just t -> pure t
        Nothing -> Lifted . TypeVar exprLoc . UnificationVar <$> makeUniqueTyVar
    -- When we have an expected type (e.g., from a user annotation), skolemise
    -- its quantified variables so they cannot unify with concrete types.
    expectedAsMono <- skolemise expected
    addType' (TermVarKey valueName) expected
    ((typedExpr, t), constraint) <- runWriter $ generateConstraints valueExpr

    let constraint' = constraint <> Equality (typeLoc expected) expectedAsMono t
    let tch = fuv t <> fuv constraint'
    debug $ "Generated constraints: " <> pretty constraint' <> " for " <> pretty valueName
    debug $ "Type: " <> pretty t

    (finalConstraint, subst) <- solveConstraint mempty tch constraint'

    case finalConstraint of
        EmptyConstraint _ -> pass
        _ -> throwError $ UnresolvedConstraint valueName finalConstraint

    let newType = substituteAll subst t

    debug $ "Substituted type: " <> pretty newType <> " from " <> pretty t <> " with " <> pretty subst

    generalized <- generalise (removeSkolems newType)

    debug $ "Generalized type: " <> pretty generalized <> " from " <> pretty newType

    pure (getExpr (substituteAll subst (SubstitutableExpr typedExpr)), generalized)

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
    substitute tv t (SubstitutableExpr (Generic.Expr (e, exprType))) = do
        let exprType' = substitute tv t exprType
        let e' =
                -- recursively apply subst to the children
                over
                    (gplate @(Monotype SourceRegion) @TypedExpr')
                    (substitute tv t)
                    (e ^. unlocated)

        SubstitutableExpr (Generic.Expr (e' <$ e, exprType'))

    -- overridden default for performance (provides a > 300% speedup by avoiding repeated traversals over potentially large expressions)
    substituteAll s (SubstitutableExpr (Generic.Expr (e, exprType))) = do
        let exprType' = substituteAll s exprType
        let e' =
                -- recursively apply subst to the children
                over
                    (gplate @(Monotype SourceRegion) @TypedExpr')
                    (substituteAll s)
                    (e ^. unlocated)

        SubstitutableExpr (Generic.Expr (e' <$ e, exprType'))
