{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Elara.TypeInfer where

import Control.Lens (Plated (..), concatMapOf, cosmosOn, to, traverseOf, view, (^.), _2)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Generics.Product
import Data.Generics.Wrapped
import Data.List.NonEmpty qualified as NonEmpty
import Data.Traversable (for)
import Elara.AST.Generic hiding (Type)
import Elara.AST.Generic qualified as Generic
import Elara.AST.Module
import Elara.AST.Name (LowerAlphaName, Name, NameLike (nameText), Qualified)
import Elara.AST.Region (Located (Located), SourceRegion, unlocated)
import Elara.AST.Select (
    LocatedAST (
        Shunted,
        Typed
    ),
 )
import Elara.AST.Shunted as Shunted
import Elara.AST.Typed as Typed
import Elara.AST.VarRef (mkGlobal')
import Elara.Data.Kind (ElaraKind (TypeKind))
import Elara.Data.Kind.Infer (InferState, inferTypeKind, initialInferState, unifyKinds)
import Elara.Data.Pretty
import Elara.Data.Unique (Unique, UniqueGen, uniqueGenToIO)
import Elara.Error (runErrorOrReport)
import Elara.Pipeline (EffectsAsPrefixOf, IsPipeline)
import Elara.Prim (primRegion)
import Elara.TypeInfer.Context
import Elara.TypeInfer.Context qualified as Context
import Elara.TypeInfer.Domain qualified as Domain
import Elara.TypeInfer.Error (TypeInferenceError (..))
import Elara.TypeInfer.Infer hiding (get)
import Elara.TypeInfer.Infer qualified as Infer
import Elara.TypeInfer.Monotype qualified as Mono
import Elara.TypeInfer.Type (Type)
import Elara.TypeInfer.Type qualified as Infer
import Elara.TypeInfer.Unique
import Polysemy hiding (transform)
import Polysemy.Error (Error, mapError, throw)
import Polysemy.State
import Print

type InferPipelineEffects = '[State Status, State InferState, Error TypeInferenceError, UniqueGen]

runInferPipeline :: IsPipeline r => Sem (EffectsAsPrefixOf InferPipelineEffects r) a -> Sem r a
runInferPipeline e = do
    s <- uniqueGenToIO initialStatus

    uniqueGenToIO
        . runErrorOrReport @TypeInferenceError
        . evalState initialInferState
        . evalState s
        $ e

inferModule ::
    forall r.
    (Members InferPipelineEffects r) =>
    Module 'Shunted ->
    Sem r (Module 'Typed)
inferModule m = do
    traverseModuleRevTopologically inferDeclaration m

inferDeclaration ::
    forall r.
    (HasCallStack, Member UniqueGen r) =>
    (Member (Error TypeInferenceError) r, Member (State Status) r, Member (State InferState) r) =>
    ShuntedDeclaration ->
    Sem r TypedDeclaration
inferDeclaration (Declaration ld) =
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
    inferDeclarationBody' declName (Value e _ (maybeExpected :: Maybe ShuntedType)) = do
        maybeExpected' <- for maybeExpected $
            \expected' -> do
                kind <- mapError KindInferError (inferTypeKind (expected' ^. _Unwrapped . unlocated))
                mapError KindInferError (unifyKinds kind TypeKind) -- expected type must be of kind Type
                astTypeToInferPolyType expected'

        e' <- inferExpression e maybeExpected'

        ctx <- Infer.getAll

        completed <- completeExpression ctx e'
        push (Annotation (mkGlobal' declName) (completed ^. _Unwrapped . _2))

        pure $ Value completed NoFieldValue NoFieldValue
    inferDeclarationBody' _ _ = error "inferDeclarationBody': not implemented"

inferExpression :: Members InferPipelineEffects r => ShuntedExpr -> Maybe (Type SourceRegion) -> Sem r TypedExpr
inferExpression e Nothing = infer e
inferExpression e (Just expectedType) = do
    ctx <- Infer.get
    wellFormedType ctx expectedType
    check e expectedType

-- inferDeclarationBody' n (Shunted.TypeDeclaration tvs ty) = do
--     ty' <-
--         traverseOf
--             unlocated
--             ( \case
--                 Renamed.Alias l -> do
--                     inferType <- astTypeToInferType l
--                     let vars' = createTypeVar <$> tvs
--                     push (Annotation (mkGlobal' n) (Infer.Alias (generatedSourceRegionFrom n) (showPretty n) vars' inferType))
--                     pure (Typed.Alias inferType)
--                 Renamed.ADT constructors -> do
--                     constructors' <- traverse (bitraverse pure (traverse astTypeToInferType)) constructors
--                     let adtType = Infer.Custom (n ^. sourceRegion) (n ^. unlocated . to nameText) (createTypeVar <$> tvs)
--                     traverse_ (\(c, b) -> addConstructorToContext tvs c b adtType) constructors'
--                     pure $ Typed.ADT constructors'
--             )
--             ty
--     kind <- mapError KindInferError (inferKind (fmap (^?! _NTypeName) (n ^. unlocated)) tvs (ty ^. unlocated))
--     pure $ Typed.TypeDeclaration tvs ty' kind

-- addConstructorToContext :: (Member (State Status) r) => [Located (Unique LowerAlphaName)] -> Located (Qualified TypeName) -> [Infer.Type SourceRegion] -> Infer.Type SourceRegion -> Sem r ()
-- addConstructorToContext typeVars ctorName ctorArgs adtType = do
--     let ctorType = foldr (\res acc -> Infer.Function (Infer.location acc) res acc) adtType ctorArgs
--     -- type Option a = Some a | None
--     -- Some : a -> Option a
--     -- None : Option a
--     let argsLoc = Infer.location <$> ctorArgs

--     -- universally quantify the type over the type variables
--     let forall' =
--             foldr
--                 ( \(Located sr u) acc ->
--                     Infer.Forall
--                         (sconcat (ctorName ^. sourceRegion :| argsLoc))
--                         sr
--                         (showPretty u)
--                         Domain.Type
--                         acc
--                 )
--                 ctorType
--                 typeVars
--     push (Annotation (mkGlobal' ctorName) forall')

createTypeVar :: Located (Unique LowerAlphaName) -> Infer.Type SourceRegion
createTypeVar (Located sr u) = Infer.VariableType sr (fmap (Just . nameText) u)

freeTypeVars :: ShuntedType -> [Located (Unique LowerAlphaName)]
freeTypeVars =
    nubOrdOn (view unlocated) -- remove duplicates, ignore location info when comparing
        . concatMapOf (cosmosOn (_Unwrapped . unlocated)) names
  where
    names :: ShuntedType' -> [Located (Unique LowerAlphaName)]
    names = \case
        TypeVar l -> [l]
        _ -> [] -- cosmos takes care of the recursion :D

-- | Like 'astTypeToInferType' but universally quantifies over the free type variables
astTypeToInferPolyType :: (Member (State Status) r, Member (Error TypeInferenceError) r) => ShuntedType -> Sem r (Infer.Type SourceRegion)
astTypeToInferPolyType l = universallyQuantify (freeTypeVars l) <$> astTypeToInferType l
  where
    universallyQuantify :: [Located (Unique LowerAlphaName)] -> Infer.Type SourceRegion -> Infer.Type SourceRegion
    universallyQuantify [] x = x
    universallyQuantify (Located sr u : us) t = Infer.Forall sr sr (fmap (Just . nameText) u) Domain.Type (universallyQuantify us t)

astTypeToInferType :: forall r. HasCallStack => (Member (State Status) r, Member (Error TypeInferenceError) r) => ShuntedType -> Sem r (Infer.Type SourceRegion)
astTypeToInferType lt@(Generic.Type (Located sr ut)) = astTypeToInferType' ut
  where
    astTypeToInferType' :: ShuntedType' -> Sem r (Infer.Type SourceRegion)
    astTypeToInferType' (TypeVar l) = pure (Infer.VariableType sr (l ^. unlocated . to (fmap (Just . nameText))))
    astTypeToInferType' UnitType = pure (Infer.Scalar sr Mono.Unit)
    astTypeToInferType' (UserDefinedType n) = do
        ctx <- Infer.get
        case Context.lookup (mkGlobal' n) ctx of
            Just ty -> pure ty
            Nothing -> throw (UserDefinedTypeNotInContext sr lt ctx)
    astTypeToInferType' (FunctionType a b) = Infer.Function sr <$> astTypeToInferType a <*> astTypeToInferType b
    astTypeToInferType' (ListType ts) = Infer.List sr <$> astTypeToInferType ts
    astTypeToInferType' (TypeConstructorApplication ctor arg) = do
        ctor' <- astTypeToInferType ctor
        arg' <- astTypeToInferType arg

        case ctor' of
            Infer.Custom{conName = ctorName, ..} -> pure $ Infer.Custom location ctorName (typeArguments ++ [arg'])
            -- Infer.Alias{..} -> pure $ Infer.Alias location name (typeArguments ++ [arg']) value
            other -> error (showColored other)
    astTypeToInferType' other = error (showColored other)

completeExpression ::
    forall r.
    (Member (State Status) r, Member UniqueGen r) =>
    Context SourceRegion ->
    TypedExpr ->
    Sem r TypedExpr
completeExpression ctx e@(Expr (y', t)) = do
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
    plate (completeExpression ctx') (Expr (y'', completed))
  where
    -- If type variables are explicitly added by the user, the algorithm doesn't re-add the forall in 'complete' (which is supposedly correct,
    -- as the types are considered "solved" in the context). However, we need to add the forall back in the final type.
    quantify :: Type SourceRegion -> Type SourceRegion
    quantify t@(Infer.Forall{}) = t
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
    unify unsolved solved = do
        -- debugPretty ("Unify" :: Text, unsolved, solved)
        case (stripForAlls unsolved, stripForAlls solved) of
            (Infer.Function{input = unsolvedInput, output = unsolvedOutput}, Infer.Function{input = solvedInput, output = solvedOutput}) -> do
                subst unsolvedInput solvedInput
                unify unsolvedInput solvedInput
                subst unsolvedOutput solvedOutput
                unify unsolvedOutput solvedOutput
            (Infer.VariableType{}, out) -> subst unsolved out
            (Infer.UnsolvedType{}, out) -> subst unsolved out
            (Infer.Scalar{}, Infer.Scalar{}) -> pass -- Scalars are always the same
            (Infer.Custom{typeArguments = unsolvedArgs}, Infer.Custom{typeArguments = solvedArgs}) -> do
                traverse_ (uncurry unify) (zip unsolvedArgs solvedArgs)
            (Infer.Tuple{tupleArguments = unsolvedArgs}, Infer.Tuple{tupleArguments = solvedArgs}) -> do
                traverse_ (uncurry unify) (NonEmpty.zip unsolvedArgs solvedArgs)
            other -> error (showPretty other)

    stripForAlls :: Type SourceRegion -> Type SourceRegion
    stripForAlls = \case
        Infer.Forall{type_} -> stripForAlls type_
        other -> other

    subst :: Type SourceRegion -> Type SourceRegion -> Sem r ()
    subst s@Infer.UnsolvedType{existential} solved = do
        -- debugPretty ("Subst" :: Text, s, solved)
        let annotation = SolvedType existential (toMonoType solved)
        push annotation
    subst a b = pass

    toMonoType :: Type SourceRegion -> Mono.Monotype
    toMonoType = \case
        Infer.Scalar{scalar} -> Mono.Scalar scalar
        Infer.Function{input, output} -> Mono.Function (toMonoType input) (toMonoType output)
        Infer.List{type_} -> Mono.List (toMonoType type_)
        Infer.UnsolvedType{existential} -> Mono.UnsolvedType existential
        Infer.VariableType{name = v} -> Mono.VariableType v
        Infer.Custom{conName = n, typeArguments = args} -> Mono.Custom n (toMonoType <$> args)
        other -> error $ "toMonoType: " <> showPretty other
