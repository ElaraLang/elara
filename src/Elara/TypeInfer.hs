{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Elara.TypeInfer where

import Data.Generics.Product
import Data.Generics.Wrapped
import Data.List.NonEmpty qualified as NonEmpty
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
import Elara.AST.Typed as Typed
import Elara.AST.VarRef (VarRef' (..), mkGlobal')
import Elara.Data.Kind (ElaraKind (..))
import Elara.Data.Kind.Infer (InferState, inferKind, inferTypeKind, initialInferState)
import Elara.Data.Unique (Unique, UniqueGen, uniqueGenToIO)
import Elara.Error (runErrorOrReport)
import Elara.Pipeline (EffectsAsPrefixOf, IsPipeline)
import Elara.TypeInfer.Context
import Elara.TypeInfer.Context qualified as Context
import Elara.TypeInfer.Domain qualified as Domain
import Elara.TypeInfer.Error (TypeInferenceError (..))
import Elara.TypeInfer.Infer hiding (get)
import Elara.TypeInfer.Infer qualified as Infer
import Elara.TypeInfer.Monotype qualified as Mono
import Elara.TypeInfer.Type (Type)
import Elara.TypeInfer.Type qualified as Infer
import Polysemy hiding (transform)
import Polysemy.Error (Error, mapError, throw)
import Polysemy.Log qualified as Log
import Polysemy.State
import Print

type InferPipelineEffects = '[Log.Log, State Status, State InferState, Error TypeInferenceError, UniqueGen]

runInferPipeline :: forall r a. IsPipeline r => Sem (EffectsAsPrefixOf InferPipelineEffects r) a -> Sem r a
runInferPipeline e = do
    s <- uniqueGenToIO initialStatus

    e
        & subsume_
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
    (Member (Error TypeInferenceError) r, Member (State Status) r, Member (State InferState) r, Member Log.Log r) =>
    ShuntedDeclaration ->
    Sem r TypedDeclaration
inferDeclaration (Declaration ld) = do
    -- debugPretty ("Infering declaration " <> showPretty (ld ^. unlocated % field' @"name"))
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

        completed <- completeExpression ctx e'
        push (Annotation (mkGlobal' declName) (completed ^. _Unwrapped % _2))

        pure $ Value completed NoFieldValue NoFieldValue (coerceValueDeclAnnotations ann)
    inferDeclarationBody' (Located _ (Qualified (NVarName _) _)) _ = error "inferDeclarationBody' NVarName"
    inferDeclarationBody' declName@(Located _ q@(Qualified (NTypeName tn) _)) (TypeDeclaration tvs (Located sr decl) ann) = do
        (kind, decl') <- mapError KindInferError (inferKind (tn <$ q) tvs decl)
        -- add the custom annotation to allow recursive types
        push
            ( Annotation
                (mkGlobal' declName)
                (Infer.Custom sr (declName ^. unlocated % to (fmap nameText)) [])
            )
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

inferExpression :: Members InferPipelineEffects r => ShuntedExpr -> Maybe (Type SourceRegion) -> Sem r TypedExpr
inferExpression e Nothing = infer e
inferExpression e (Just expectedType) = do
    (Expr (l, _)) <- check e expectedType
    pure (Expr (l, expectedType))

createTypeVar :: Located (Unique LowerAlphaName) -> Infer.Type SourceRegion
createTypeVar (Located sr u) = Infer.VariableType sr (fmap (Just . nameText) u)

universallyQuantify :: [Located (Unique LowerAlphaName)] -> Infer.Type SourceRegion -> Infer.Type SourceRegion
universallyQuantify [] x = x
universallyQuantify (Located sr u : us) t =
    Infer.Forall sr sr (fmap (Just . nameText) u) Domain.Type (universallyQuantify us t)

-- | Like 'astTypeToInferType' but universally quantifies over the free type variables
astTypeToInferPolyType :: (HasCallStack, Member (State Status) r, Member (Error TypeInferenceError) r) => KindedType -> Sem r (Infer.Type SourceRegion, ElaraKind)
astTypeToInferPolyType l = first (universallyQuantify (freeTypeVars l)) <$> astTypeToInferType l

astTypeToInferType :: forall r. HasCallStack => (Member (State Status) r, Member (Error TypeInferenceError) r) => KindedType -> Sem r (Infer.Type SourceRegion, ElaraKind)
astTypeToInferType lt@(Generic.Type (Located sr ut, kind)) = astTypeToInferType' ut
  where
    astTypeToInferType' :: KindedType' -> Sem r (Infer.Type SourceRegion, ElaraKind)
    astTypeToInferType' (TypeVar l) = pure (Infer.VariableType sr (l ^. unlocated % to (fmap (Just . nameText))), kind)
    astTypeToInferType' UnitType = pure (Infer.Scalar sr Mono.Unit, kind)
    astTypeToInferType' (UserDefinedType n) = do
        ctx <- Infer.get
        case Context.lookup (mkGlobal' n) ctx of
            Just ty -> pure (ty, kind)
            Nothing -> throw (UserDefinedTypeNotInContext sr lt ctx)
    astTypeToInferType' (FunctionType a b) = do
        (a', _) <- astTypeToInferType a
        (b', _) <- astTypeToInferType b
        pure (Infer.Function sr a' b', kind)
    astTypeToInferType' (ListType ts) = do
        (ts', _) <- astTypeToInferType ts
        pure (Infer.List sr ts', kind)
    astTypeToInferType' (TypeConstructorApplication ctor arg) = do
        (ctor', _) <- astTypeToInferType ctor
        (arg', _) <- astTypeToInferType arg

        -- apply the type argument to the constructor
        -- this will discharge one single forall and one single function arrow
        let applyTypeArg ctor'' arg'' = base
              where
                base = case ctor'' of
                    Infer.Custom{conName = ctorName, ..} -> Infer.Custom sr ctorName (typeArguments ++ [arg''])
                    Infer.Function{output} -> output
                    Infer.Forall{..} ->
                        -- apply a substitution to the forall
                        Infer.substituteType name arg'' (rec' type_)
                    other -> error (showPretty (other, arg''))
                rec' ctor'' = case ctor'' of
                    Infer.Function{output} -> output
                    Infer.Forall{type_, ..} -> Infer.Forall{type_ = rec' type_, ..}
                    other -> other

        pure (applyTypeArg ctor' arg', kind)
    astTypeToInferType' other = error (showColored other)

completeExpression ::
    forall r.
    (HasCallStack, Member (State Status) r, Member UniqueGen r, Member (Error TypeInferenceError) r) =>
    Context SourceRegion ->
    TypedExpr ->
    Sem r TypedExpr
completeExpression ctx (Expr (y', t)) = do
    completed <- quantify <$> complete ctx t
    unify t completed

    ctx' <- Infer.getAll
    y'' <-
        traverseOf
            unlocated
            ( \case
                TypeApplication f t' -> TypeApplication f <$> complete ctx' t'
                o -> pure o
            )
            y'
    completedExprs <- traverseOf gplate (completeExpression ctx') (Expr (y'', completed))
    traverseOf gplate (completePattern ctx') completedExprs
  where
    completePattern :: HasCallStack => Context SourceRegion -> TypedPattern -> Sem r TypedPattern
    completePattern ctx (Pattern (p', t)) = do
        completed <- quantify <$> complete ctx t
        wellFormedType ctx completed
        unify t completed
        ctx' <- Infer.getAll
        traverseOf gplate (completePattern ctx') (Pattern (p', completed))

    -- If type variables are explicitly added by the user, the algorithm doesn't re-add the forall in 'complete' (which is supposedly correct,
    -- as the types are considered "solved" in the context). However, we need to add the forall back in the final type.
    quantify :: Type SourceRegion -> Type SourceRegion
    quantify fa@(Infer.Forall{}) = fa
    quantify x = do
        let ftvs = Infer.freeTypeVars x

        foldr (\(Located l tv) acc -> Infer.Forall l l tv Domain.Type acc) x ftvs
    {-
    Unifies completed types with unsolved ones. It assumes that the types are of the same shape, excluding quantifiers.

    unify (a? -> b?) (forall a. a -> a) creates 2 constraints:
        - a? = a
        - b? = a

    unify (a? -> b?) (forall a. forall b. a -> b) creates 2 constraints:
        - a? = a
        - b? = b

    -}
    unify :: Type SourceRegion -> Type SourceRegion -> Sem r ()
    unify unsolved solved = case (Infer.stripForAll unsolved, Infer.stripForAll solved) of
        (Infer.Function{input = unsolvedInput, output = unsolvedOutput}, Infer.Function{input = solvedInput, output = solvedOutput}) -> do
            subst unsolvedInput solvedInput
            unify unsolvedInput solvedInput
            subst unsolvedOutput solvedOutput
            unify unsolvedOutput solvedOutput
        (Infer.VariableType{}, out) -> subst unsolved out
        (Infer.UnsolvedType{}, out) -> subst unsolved out
        (Infer.Scalar{}, Infer.Scalar{}) -> pass -- Scalars are always the same
        (Infer.Custom{typeArguments = unsolvedArgs}, Infer.Custom{typeArguments = solvedArgs}) -> traverse_ (uncurry unify) (zip unsolvedArgs solvedArgs)
        (Infer.Tuple{tupleArguments = unsolvedArgs}, Infer.Tuple{tupleArguments = solvedArgs}) -> traverse_ (uncurry unify) (NonEmpty.zip unsolvedArgs solvedArgs)
        (Infer.List{type_ = unsolvedType}, Infer.List{type_ = solvedType}) -> unify unsolvedType solvedType
        other -> error (showPretty other)

    subst :: Type SourceRegion -> Type SourceRegion -> Sem r ()
    subst Infer.UnsolvedType{existential} solved = do
        let annotation = SolvedType existential (toMonoType solved)
        push annotation
    subst _ _ = pass

    toMonoType :: Type SourceRegion -> Mono.Monotype
    toMonoType = \case
        Infer.Scalar{scalar} -> Mono.Scalar scalar
        Infer.Function{input, output} -> Mono.Function (toMonoType input) (toMonoType output)
        Infer.List{type_} -> Mono.List (toMonoType type_)
        Infer.Tuple{tupleArguments} -> Mono.Tuple (toMonoType <$> tupleArguments)
        Infer.UnsolvedType{existential} -> Mono.UnsolvedType existential
        Infer.VariableType{name = v} -> Mono.VariableType v
        Infer.Custom{conName = n, typeArguments = args} -> Mono.Custom n (toMonoType <$> args)
        other -> error $ "toMonoType: " <> showPretty other
