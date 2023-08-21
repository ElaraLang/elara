{-# LANGUAGE RecordWildCards #-}

module Elara.TypeInfer where

import Control.Lens (Plated (..), concatMapOf, cosmosOn, to, traverseOf, view, (^.), _2, _3)
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.Generics.Product
import Data.Generics.Sum
import Data.Generics.Wrapped
import Data.List.NonEmpty qualified as NonEmpty
import Data.Traversable (for)
import Elara.AST.Generic hiding (Type)
import Elara.AST.Generic qualified as Generic
import Elara.AST.Module
import Elara.AST.Name (LowerAlphaName, Name, NameLike (nameText), Qualified, _LowerAlphaName)
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
import Elara.Data.Unique (Unique, uniqueGenToIO, uniqueToText)
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
import Polysemy hiding (transform)
import Polysemy.Error (Error, mapError, throw)
import Polysemy.State
import Print

type InferPipelineEffects = '[State Status, State InferState, Error TypeInferenceError]

runInferPipeline :: IsPipeline r => Sem (EffectsAsPrefixOf InferPipelineEffects r) a -> Sem r a
runInferPipeline e = do
    s <- uniqueGenToIO initialStatus

    runErrorOrReport @TypeInferenceError
        . evalState initialInferState
        . evalState s
        $ e

inferModule ::
    forall r.
    HasCallStack =>
    (Member (Error TypeInferenceError) r, Member (State Status) r, Member (State InferState) r) =>
    Module 'Shunted ->
    Sem r (Module 'Typed)
inferModule = traverseModuleRevTopologically inferDeclaration

inferDeclaration ::
    forall r.
    HasCallStack =>
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

        -- add the expected type as an annotation
        -- this makes recursive definitions work (although perhaps it shouldn't)
        case maybeExpected' of
            Just expectedType -> push (Annotation (mkGlobal' declName) expectedType)
            Nothing -> do
                exist <- fresh
                push (UnsolvedType exist)
                push (Annotation (mkGlobal' declName) (Infer.UnsolvedType primRegion exist))

        e' <- inferExpression e maybeExpected'

        ctx <- Infer.getAll

        completed <- completeExpression ctx e'

        whenNothing_ Nothing (push (Annotation (mkGlobal' declName) (completed ^. _Unwrapped . _2)))

        pure $ Value completed NoFieldValue NoFieldValue
    inferDeclarationBody' _ _ = error "inferDeclarationBody': not implemented"

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
createTypeVar (Located sr u) = Infer.VariableType sr (nameText <$> u)

freeTypeVars :: ShuntedType -> [Located (Unique LowerAlphaName)]
freeTypeVars =
    nubOrd -- remove duplicates, ignore location info when comparing
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
    universallyQuantify (Located sr u : us) t = Infer.Forall sr sr (fmap nameText u) Domain.Type (universallyQuantify us t)

astTypeToInferType :: forall r. HasCallStack => (Member (State Status) r, Member (Error TypeInferenceError) r) => ShuntedType -> Sem r (Infer.Type SourceRegion)
astTypeToInferType lt@(Generic.Type (Located sr ut)) = astTypeToInferType' ut
  where
    astTypeToInferType' :: ShuntedType' -> Sem r (Infer.Type SourceRegion)
    astTypeToInferType' (TypeVar l) = pure (Infer.VariableType sr (l ^. unlocated . to (fmap nameText)))
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

-- inferPattern ::
--     forall r.
--     (Member (Error TypeInferenceError) r, Member (State Status) r) =>
--     Shunted.Pattern ->
--     Maybe (Infer.Type SourceRegion) ->
--     Sem r Typed.Pattern
-- inferPattern p@(Shunted.Pattern lp) expected = do
--     (ty', p') <-
--         Infer.inferPattern
--             p
--             (traverseOf unlocated inferPattern' lp)
--     ctx <- Infer.get
--     whenJust expected (checkPattern p) -- check that the inferred type is a subtype of the expected type
--     let completedType = complete ctx (fromMaybe ty' expected)
--     -- We set the type of the expression to the expected type if it was given, otherwise we use the inferred type

--     pure $ Typed.Pattern (p', completedType)
--   where
--     inferPattern' :: Shunted.Pattern' -> Sem r Typed.Pattern'
--     inferPattern' (Shunted.VarPattern v) = pure (Typed.VarPattern v)
--     inferPattern' Shunted.WildcardPattern = pure Typed.WildcardPattern
--     inferPattern' (Shunted.ListPattern l) = Typed.ListPattern <$> traverse (`inferPattern` Nothing) l
--     inferPattern' (Shunted.ConsPattern l r) = Typed.ConsPattern <$> (`inferPattern` Nothing) l <*> (`inferPattern` Nothing) r
--     inferPattern' other = error (showColored other)

inferExpression ::
    forall r.
    (Member (Error TypeInferenceError) r, Member (State Status) r) =>
    ShuntedExpr ->
    Maybe (Infer.Type SourceRegion) ->
    Sem r TypedExpr
inferExpression e expected = do
    e' <- infer e
    case expected of
        Just ex -> check e ex
        Nothing -> pure e'

completeExpression ::
    forall r.
    Member (State Status) r =>
    Context SourceRegion ->
    TypedExpr ->
    Sem r TypedExpr
completeExpression ctx (Expr (y', t)) = do
    let completed = complete ctx t
    unify t completed

    ctx' <- Infer.getAll

    plate (completeExpression ctx') (Expr (y', completed))
  where
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
    unify unsolved solved = case (stripForAlls unsolved, stripForAlls solved) of
        (Infer.Function{input = unsolvedInput, output = unsolvedOutput}, Infer.Function{input = solvedInput, output = solvedOutput}) -> do
            subst unsolvedInput solvedInput
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
    subst Infer.UnsolvedType{existential} solved = do
        let annotation = SolvedType existential (toMonoType solved)
        push annotation
    subst _ _ = pass

    toMonoType :: Type SourceRegion -> Mono.Monotype
    toMonoType = \case
        Infer.Scalar{scalar} -> Mono.Scalar scalar
        Infer.Function{input, output} -> Mono.Function (toMonoType input) (toMonoType output)
        Infer.List{type_} -> Mono.List (toMonoType type_)
        Infer.UnsolvedType{existential} -> Mono.UnsolvedType existential
        Infer.VariableType{name = v} -> Mono.VariableType v
        Infer.Custom{conName = n, typeArguments = args} -> Mono.Custom n (toMonoType <$> args)
        other -> error $ "toMonoType: " <> showPretty other
