{-# LANGUAGE BlockArguments #-}

module Elara.TypeInfer where

import Data.Generics.Product
import Data.Generics.Wrapped (_Unwrapped)
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Effectful.Writer.Static.Local (listen, runWriter)
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
import Elara.AST.Name (LowerAlphaName, ModuleName, NameLike (nameText), Qualified (..), VarName)
import Elara.AST.Region (Located (Located), SourceRegion, unlocated)
import Elara.AST.Select (
    LocatedAST (
        Shunted,
        Typed
    ),
 )
import Elara.AST.Shunted as Shunted
import Elara.AST.Typed as Typed
import Elara.Data.Kind.Infer (InferState, KindInferError, inferKind, inferTypeKind, initialInferState)
import Elara.Data.Pretty
import Elara.Data.Unique (Unique)
import Elara.Data.Unique.Effect
import Elara.Error (runErrorOrReport)
import Elara.Logging (StructuredDebug, debug)
import Elara.Pipeline (EffectsAsPrefixOf, IsPipeline)
import Elara.Query (Query (..))
import Elara.Query.Effects
import Elara.Rename.Error
import Elara.Shunt.Error (ShuntError)
import Elara.TypeInfer.ConstraintGeneration
import Elara.TypeInfer.Convert (TypeConvertError, astTypeToGeneralisedInferType, astTypeToInferType, astTypeToInferTypeWithKind)
import Elara.TypeInfer.Environment (InferError, TypeEnvKey (..), addType', emptyLocalTypeEnvironment, emptyTypeEnvironment)
import Elara.TypeInfer.Ftv (Fuv (..))
import Elara.TypeInfer.Generalise
import Elara.TypeInfer.Monad
import Elara.TypeInfer.Type (Constraint (..), Monotype (..), Polytype (..), Substitutable (..), Type (..), TypeVariable (..))
import Elara.TypeInfer.Unique (UniqueTyVar, makeUniqueTyVar)
import Print (showPretty)
import Relude.Extra.Type (type (++))
import Rock qualified
import TODO

type InferPipelineEffects r =
    ( StructuredDebug :> r
    , State InferState :> r
    , UniqueGen :> r
    , Error (UnifyError SourceRegion) :> r
    , Error TypeConvertError :> r
    , Error KindInferError :> r
    , Infer SourceRegion r
    )

runGetTypeCheckedModuleQuery ::
    ModuleName ->
    Eff
        ( ConsQueryEffects
            '[ Rock.Rock Elara.Query.Query
             ]
        )
        (Module 'Typed)
runGetTypeCheckedModuleQuery mn = do
    shunted <- runErrorOrReport @ShuntError $ Rock.fetch $ Elara.Query.ShuntedModule mn
    r <- runInferEffects $ evalState initialInferState (inferModule shunted)
    pure (fst r)

runGetTypeOfQuery :: TypeEnvKey -> Eff (ConsQueryEffects '[Rock.Rock Elara.Query.Query]) (Type SourceRegion)
runGetTypeOfQuery key =
    case key of
        TermVarKey varName -> do
            let mod = varName.qualifier
            shunted <-
                runErrorOrReport @ShuntError $
                    Rock.fetch $
                        ShuntedModule mod
            let decl =
                    shunted ^.. _Unwrapped % unlocated

            _

runInferEffects ::
    forall r a loc.
    Pretty loc =>
    Eff
        ( InferEffectsCons
            loc
            ( Error (UnifyError loc)
                ': Error KindInferError
                ': Error TypeConvertError
                ': Rock.Rock Elara.Query.Query
                ': ConsQueryEffects r
            )
        )
        a ->
    Eff (ConsQueryEffects (Rock.Rock Elara.Query.Query ': r)) (a, Constraint loc)
runInferEffects e = do
    e
        & runErrorOrReport @(InferError _)
        . runErrorOrReport @(UnifyError _)
        . runErrorOrReport @KindInferError
        . runErrorOrReport @TypeConvertError
        . evalState emptyLocalTypeEnvironment
        . evalState emptyTypeEnvironment
        . runWriter @(Constraint _)
        . inject

inferModule ::
    forall r.
    (InferPipelineEffects r, Infer SourceRegion r) =>
    Module 'Shunted ->
    Eff r (Module 'Typed)
inferModule m = do
    traverseModuleRevTopologically inferDeclaration m

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
            pure (Value name typedExpr NoFieldValue (Polytype polytype) (Generic.coerceValueDeclAnnotations annotations))
        TypeDeclaration name tyVars body anns -> do
            (kind, decl') <- inferKind (name ^. unlocated) tyVars (body ^. unlocated)
            case decl' of
                Generic.Alias t -> do
                    _ <- astTypeToInferType t
                    -- addType' (TypeVarKey (name ^. unlocated)) t'
                    todo
                Generic.ADT ctors -> do
                    let tyVars' = fmap createTypeVar tyVars
                    let typeConstructorType = TypeConstructor (name ^. unlocated) (fmap (TypeVar . UnificationVar) tyVars')

                    let inferCtor (ctorName, t :: [KindedType]) = do
                            t' <- traverse astTypeToInferTypeWithKind t
                            let ctorType =
                                    foldr (Function . fst) typeConstructorType t'
                            addType' (DataConKey (ctorName ^. unlocated)) (Polytype (Forall tyVars' EmptyConstraint ctorType))

                            pure (ctorName, t')

                    ctors' <- traverse inferCtor ctors
                    let ann' =
                            Generic.TypeDeclAnnotations
                                { infixTypeDecl =
                                    Generic.coerceInfixDeclaration
                                        <$> anns.infixTypeDecl
                                , kindAnn = kind
                                }
                    pure
                        ( TypeDeclaration
                            name
                            (zipWith (<$) tyVars' tyVars)
                            (Generic.ADT ctors' <$ body)
                            ann'
                        )

createTypeVar :: Located (Unique LowerAlphaName) -> UniqueTyVar
createTypeVar (Located _ u) = fmap (Just . nameText) u

inferValue ::
    forall r.
    ( HasCallStack
    , UniqueGen :> r
    , Infer SourceRegion r
    , Error (UnifyError SourceRegion) :> r
    ) =>
    Qualified VarName ->
    ShuntedExpr ->
    Maybe (Type SourceRegion) ->
    Eff r (TypedExpr, Polytype SourceRegion)
inferValue valueName valueExpr expectedType = do
    -- generate
    expected <- case expectedType of
        Just t -> pure t
        Nothing -> Lifted . TypeVar . UnificationVar <$> makeUniqueTyVar
    -- When we have an expected type (e.g., from a user annotation), skolemise
    -- its quantified variables so they cannot unify with concrete types.
    expectedAsMono <- skolemise expected
    debug $ "Skolemised expected type of" <+> pretty valueName <+> ": " <> pretty expectedAsMono
    addType' (TermVarKey valueName) expected
    ((typedExpr, t), constraint) <- listen $ generateConstraints valueExpr

    let constraint' = constraint <> Equality expectedAsMono t
    let tch = fuv t <> fuv constraint'
    debug $ "Generated constraints: " <> pretty constraint' <> " for " <> pretty valueName
    debug $ "Type: " <> pretty t
    debug $ "tch: " <> pretty tch

    (finalConstraint, subst) <- solveConstraint mempty tch constraint'

    when (finalConstraint /= EmptyConstraint) do
        throwError (UnresolvedConstraint valueName finalConstraint)

    let newType = substituteAll subst t

    debug $ "Substituted type: " <> pretty newType <> " from " <> pretty t <> " with " <> pretty subst

    generalized <- generalise (removeSkolems newType)

    pure (getExpr (substituteAll subst (SubstitutableExpr typedExpr)), generalized)

-- Replace all quantified variables in a type scheme with rigid skolem variables.
-- This prevents ill-typed programs from unifying annotated polymorphic variables
-- with concrete types during checking.
skolemise :: forall r. Type SourceRegion -> Eff r (Monotype SourceRegion)
skolemise = \case
    Lifted t -> pure t
    Polytype (Forall tyVars _ t) -> do
        -- Build a substitution mapping each quantified variable α to a rigid skolem #α
        let pairs = zip (fmap (view typed) tyVars) (TypeVar . SkolemVar <$> tyVars)
        pure $ foldl' (\acc (tv, rep) -> substitute tv rep acc) t pairs

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
