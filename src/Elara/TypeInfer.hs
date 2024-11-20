{-# LANGUAGE BlockArguments #-}

module Elara.TypeInfer where

import Data.Containers.ListUtils (nubOrd)
import Data.Generics.Product
import Data.Generics.Wrapped
import Data.List ((\\))
import Data.Map qualified as Map
import Elara.AST.Generic ()
import Elara.AST.Generic hiding (Type)
import Elara.AST.Generic qualified as Generic
import Elara.AST.Generic.Common
import Elara.AST.Kinded
import Elara.AST.Module
import Elara.AST.Name (LowerAlphaName, Name (..), NameLike (nameText), Qualified (..))
import Elara.AST.Region (IgnoreLocation (..), Located (Located), SourceRegion, unlocated)
import Elara.AST.Select (
    LocatedAST (
        Shunted,
        Typed
    ),
 )
import Elara.AST.Shunted as Shunted
import Elara.AST.StripLocation (StripLocation (..))
import Elara.AST.Typed as Typed
import Elara.AST.VarRef (VarRef' (..), mkGlobal')
import Elara.Data.Kind (ElaraKind (..))
import Elara.Data.Kind.Infer (InferState, inferKind, inferTypeKind, initialInferState)
import Elara.Data.Unique (Unique, UniqueGen, uniqueGenToIO)
import Elara.Error (runErrorOrReport)
import Elara.Logging (StructuredDebug, debug, debugWith, structuredDebugToLog)
import Elara.Pipeline (EffectsAsPrefixOf, IsPipeline)
import Elara.Prim (fullListName, primRegion)
import Elara.TypeInfer.Context
import Elara.TypeInfer.Context qualified as Context
import Elara.TypeInfer.Domain qualified as Domain
import Elara.TypeInfer.Error (TypeInferenceError (..))
import Elara.TypeInfer.Infer hiding (get)
import Elara.TypeInfer.Infer qualified as Infer
import Elara.TypeInfer.Kind
import Elara.TypeInfer.Monotype qualified as Mono
import Elara.TypeInfer.Type (Type)
import Elara.TypeInfer.Type qualified as Infer
import Polysemy hiding (transform)
import Polysemy.Error (Error, mapError, throw)
import Polysemy.State
import Print

type InferPipelineEffects = '[StructuredDebug, State Status, State InferState, Error TypeInferenceError, UniqueGen]

runInferPipeline :: forall r a. IsPipeline r => Sem (EffectsAsPrefixOf InferPipelineEffects r) a -> Sem r a
runInferPipeline e = do
    s <- uniqueGenToIO initialStatus

    e
        & subsume_
        & structuredDebugToLog
        & evalState initialInferState
        & evalState s
        & runErrorOrReport @TypeInferenceError
        & uniqueGenToIO

inferModule ::
    forall r.
    Members InferPipelineEffects r =>
    Module 'Shunted ->
    Sem r (Module 'Typed, Map (Qualified Name) (Type SourceRegion))
inferModule m = do
    m' <- traverseModuleRevTopologically inferDeclaration m
    ctx <- Infer.get
    let annotations =
            Map.fromList $
                reverse
                    ( flip mapMaybe ctx $ \case
                        Annotation (Global (IgnoreLocation (Located _ n))) t -> Just (n, t)
                        _ -> Nothing
                    )
    pure (m', annotations)

inferDeclaration ::
    forall r.
    (HasCallStack, Member UniqueGen r) =>
    (Member (Error TypeInferenceError) r, Member (State Status) r, Member (State InferState) r, Member StructuredDebug r) =>
    ShuntedDeclaration ->
    Sem r TypedDeclaration
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
                            (inferDeclarationBody' (d' ^. field' @"name"))
                            ldb
                pure (Declaration' (d' ^. field' @"moduleName") (d' ^. field' @"name") db')
            )
            ld
  where
    inferDeclarationBody' ::
        HasCallStack =>
        Located (Qualified Name) ->
        ShuntedDeclarationBody' ->
        Sem r TypedDeclarationBody'
    inferDeclarationBody' declName (Value e _ (maybeExpected :: Maybe ShuntedType) ann) = do
        maybeExpected' <- case maybeExpected of
            Just expected' -> do
                expected' <- mapError KindInferError (inferTypeKind expected')
                -- mapError KindInferError (unifyKinds kind TypeKind) -- expected type must be of kind Type
                (expectedPoly, expectedKind) <- astTypeToInferPolyType expected'
                push (Annotation (mkGlobal' declName) expectedPoly)
                pure expectedPoly
            Nothing -> do
                -- if no expected type is given, we create a fresh type variable
                -- this is useful for top-level declarations, where we don't know the type yet
                -- but we still want to infer it
                f <- fresh
                let y = Infer.UnsolvedType (e ^. exprLocation) f
                push (UnsolvedType f)
                push (Annotation (mkGlobal' declName) y)
                pure y

        e' <- inferExpression e (Just maybeExpected')

        ctx <- Infer.getAll
        debugPretty $ simplifyContext (stripLocation ctx)

        completed <- completeExpression ctx e'
        push (Annotation (mkGlobal' declName) (completed ^. _Unwrapped % _2))

        pure $ Value completed NoFieldValue NoFieldValue (coerceValueDeclAnnotations ann)
    inferDeclarationBody' (Located _ (Qualified (NVarName _) _)) _ = error "inferDeclarationBody' NVarName"
    inferDeclarationBody' declName@(Located _ q@(Qualified (NTypeName tn) _)) (TypeDeclaration tvs (Located sr decl) ann) = do
        (kind, decl') <- mapError KindInferError (inferKind (tn <$ q) tvs decl)
        -- add the custom annotation to allow recursive types
        -- push
        --     ( Annotation
        --         (mkGlobal' declName)
        --         (Infer.Custom sr (declName ^. unlocated % to (fmap nameText)) [])
        --     )
        case decl' of
            Alias t -> do
                (t', k) <- astTypeToInferType t
                push (Annotation (mkGlobal' declName) t')
                let ann' = TypeDeclAnnotations{infixTypeDecl = coerceInfixDeclaration <$> ann.infixTypeDecl, kindAnn = k}
                pure $ TypeDeclaration tvs (Located sr (Alias (t', kind))) ann'
            ADT ctors' -> do
                let tvs' = map createTypeVar tvs
                let adtWithTvs = Infer.Custom sr (declName ^. unlocated % to (fmap nameText)) tvs'

                let inferCtor (ctorName, t :: [KindedType]) = do
                        t' <- traverse astTypeToInferType t
                        let ctorType =
                                universallyQuantify
                                    tvs
                                    (foldr (Infer.Function sr . fst) adtWithTvs t')
                        push (Annotation (mkGlobal' ctorName) ctorType)

                        pure (ctorName, t')
                ctors' <- traverse inferCtor ctors'
                let ann' = TypeDeclAnnotations{infixTypeDecl = coerceInfixDeclaration <$> ann.infixTypeDecl, kindAnn = kind}
                pure $ TypeDeclaration tvs (Located sr (ADT ctors')) ann'

inferFreshExpression ::
    Members InferPipelineEffects r => ShuntedExpr -> Sem r TypedExpr
inferFreshExpression e = do
    -- if no expected type is given, we create a fresh type variable
    -- this is useful for top-level declarations, where we don't know the type yet
    -- but we still want to infer it
    f <- fresh
    let y = Infer.UnsolvedType (e ^. exprLocation) f
    push (UnsolvedType f)
    inferExpression e (Just y)

inferExpression :: Members InferPipelineEffects r => ShuntedExpr -> Maybe (Type SourceRegion) -> Sem r TypedExpr
inferExpression e Nothing = infer e
inferExpression e (Just expectedType) = do
    (Expr (l, f)) <- check e expectedType
    pure (Expr (l, f))

createTypeVar :: Located (Unique LowerAlphaName) -> Infer.Type SourceRegion
createTypeVar (Located sr u) = Infer.VariableType sr (fmap (Just . nameText) u)

completeExpression ::
    forall r.
    (HasCallStack, Member (State Status) r, Member UniqueGen r, Member (Error TypeInferenceError) r, Member StructuredDebug r) =>
    Context SourceRegion ->
    TypedExpr ->
    Sem r TypedExpr
completeExpression ctx (Expr (y', t)) = debugWith ("completeExpression: " <> showPretty (y', t)) $ do
    ctx' <- Infer.getAll

    let solveFromGraph' t = do
            graph <- unifyContextGraph $ simplifyContext (stripLocation ctx')

            -- TODO: solveFromGraph needs to return all the neighbours so we can unify all of them
            -- unify k with m? -> n?
            let solved = solveFromGraph graph (stripLocation t)
            debug ("solveFromGraph solved: " <> showPretty (t, solved))
            all <- fmap stripLocation <$> Infer.getAll
            let fromGraph = graphToContext graph
            let c = fromGraph <> all
            completed <- complete c solved
            debug (showPretty (t, completed))
            pure (t.location <$ completed)

    completedType <- solveFromGraph' t

    -- debug ("completeExpression ctx': " <> showPretty (nubOrd ctx', graph))
    fixedCompletedExpr <-
        traverseOf
            unlocated
            ( \case
                TypeApplication f t' -> TypeApplication f <$> solveFromGraph' t'
                Lambda p e -> do
                    -- stupid
                    p' <- traverseOf (unlocated % _Unwrapped) (\(n, t) -> (n,) <$> solveFromGraph' t) p
                    pure (Lambda p' e)
                o -> pure o
            )
            y'
    debug ("completeExpressionDone: " <> showPretty (fixedCompletedExpr, completedType))
    recursivelyCompletedExpr <- traverseOf gplate (completeExpression ctx') (Expr (fixedCompletedExpr, completedType))
    traverseOf gplate (completePattern ctx') recursivelyCompletedExpr
  where
    completePattern :: HasCallStack => Context SourceRegion -> TypedPattern -> Sem r TypedPattern
    completePattern ctx (Pattern (p', t)) = do
        completed <- complete ctx t
        wellFormedType ctx completed
        unify t completed
        ctx' <- Infer.getAll
        traverseOf gplate (completePattern ctx') (Pattern (p', completed))

    -- -- If type variables are explicitly added by the user, the algorithm doesn't re-add the forall in 'complete' (which is supposedly correct,
    -- -- as the types are considered "solved" in the context). However, we need to add the forall back in the final type.
    -- quantify :: Type SourceRegion -> Type SourceRegion
    -- quantify fa@(Infer.Forall{}) = fa
    -- quantify x = do
    --     let ftvs = Infer.freeTypeVars x

    --     foldr (\(Located l tv) acc -> Infer.Forall l l tv Domain.Type acc) x ftvs
    {-
    Unifies completed types with unsolved ones. It assumes that the types are of the same shape, excluding quantifiers.

    unify (a? -> b?) (forall a. a -> a) creates 2 constraints:
        - a? = a
        - b? = a

    unify (a? -> b?) (forall a. forall b. a -> b) creates 2 constraints:
        - a? = a
        - b? = b

    Unification can get complex as it has to fully traverse down the "tree" of solved types:
    Consider the context
    @
    k = l?
    , ➤ k: Type
    , r_1: forall _9 . _9 -> _9
    , g = f?
    , f = h?
    , r_1: e?
    , e = h? -> i?
    , h = i?
    , i?
    , Main.bbb: d?
    , d = l? -> m?
    , l = m?
    , m?
    @

    and suppose we want to solve the type @f@:
    @
    f = h?
    h = i?
    i?
    @
    Initially, one might think that we would get stuck here, as we don't know what @i@ is.
    However, we can figure out a lot more if we use the other parts of the context:
    @
    r_1 : forall _9 . _9 -> _9
    r_1 : e?
    -------------------------
    e = forall _9 . _9 -> _9

    e = h? -> i?
    h? = i?
    ------------
    e = i? -> i

    e = i? -> i?
    e = forall _9 . _9 -> _9
    ------------------------
    i = _9
    @
    -}
    unify :: Type SourceRegion -> Type SourceRegion -> Sem r ()
    unify unsolved solved | unsolved == solved = pass
    unify unsolved solved = debugWith ("unify: " <> showPretty (unsolved, solved)) $
        case (Infer.stripForAll unsolved, Infer.stripForAll solved) of
            (Infer.Function{input = unsolvedInput, output = unsolvedOutput}, Infer.Function{input = solvedInput, output = solvedOutput}) -> do
                subst unsolvedInput solvedInput
                unify unsolvedInput solvedInput
                subst unsolvedOutput solvedOutput
                unify unsolvedOutput solvedOutput
            (Infer.VariableType{}, out) -> subst unsolved out
            (Infer.UnsolvedType{}, out) -> do
                subst unsolved out
                ctx <- Infer.getAll
                debug ("unifySolved: " <> showPretty (Context.solveType ctx unsolved, Context.solveType ctx out))
            (Infer.Scalar{}, Infer.Scalar{}) -> pass -- Scalars are always the same
            (Infer.Custom{typeArguments = unsolvedArgs}, Infer.Custom{typeArguments = solvedArgs}) -> traverse_ (uncurry unify) (zip unsolvedArgs solvedArgs)
            other -> error (showPretty other)

    subst :: Type SourceRegion -> Type SourceRegion -> Sem r ()
    subst t@Infer.UnsolvedType{existential} solved = do
        let annotation = SolvedType existential (toMonoType solved)
        debug ("subst: " <> showPretty annotation)
        ctx <- Infer.get
        let solvedT = Context.solveType ctx t
        push annotation
        ctx <- Infer.get
        unify solvedT (Context.solveType ctx t)
    subst a b = debug ("subst!: " <> showPretty (a, b))

    toMonoType :: Type SourceRegion -> Mono.Monotype
    toMonoType = \case
        Infer.Scalar{scalar} -> Mono.Scalar scalar
        Infer.Function{input, output} -> Mono.Function (toMonoType input) (toMonoType output)
        Infer.UnsolvedType{existential} -> Mono.UnsolvedType existential
        Infer.VariableType{name = v} -> Mono.VariableType v
        Infer.Custom{conName = n, typeArguments = args} -> Mono.Custom n (toMonoType <$> args)
        other -> error $ "toMonoType: " <> showPretty other
