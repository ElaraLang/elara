{-# LANGUAGE RecordWildCards #-}

module Elara.TypeInfer where

import Control.Lens (concatMapOf, cosmosOn, to, traverseOf, view, (^.), (^?!), _3)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Traversable (for)
import Elara.AST.Lenses (HasDeclarationBody (..))
import Elara.AST.Module
import Elara.AST.Name (LowerAlphaName, Name, Qualified, TypeName, nameText, _LowerAlphaName, _NTypeName)
import Elara.AST.Region (Located (Located), SourceRegion, generatedSourceRegionFrom, sourceRegion, unlocated, withLocationOf)
import Elara.AST.Renamed qualified as Renamed
import Elara.AST.Select
import Elara.AST.Shunted as Shunted
import Elara.AST.Typed as Typed
import Elara.AST.VarRef (mkGlobal')
import Elara.Data.Kind (ElaraKind (TypeKind))
import Elara.Data.Kind.Infer (InferState, inferKind, inferTypeKind, unifyKinds)
import Elara.Data.Unique (Unique, uniqueToText, uniqueVal)
import Elara.TypeInfer.Context
import Elara.TypeInfer.Context qualified as Context
import Elara.TypeInfer.Domain qualified as Domain
import Elara.TypeInfer.Error (TypeInferenceError (KindInferError, UserDefinedTypeNotInContext))
import Elara.TypeInfer.Infer hiding (get, inferPattern)
import Elara.TypeInfer.Infer qualified as Infer
import Elara.TypeInfer.Monotype qualified as Mono
import Elara.TypeInfer.Type ()
import Elara.TypeInfer.Type qualified as Infer
import Polysemy hiding (transform)
import Polysemy.Error (Error, mapError, throw)
import Polysemy.State
import Print
import TODO (todo)

inferModule ::
    forall r.
    HasCallStack =>
    (Member (Error TypeInferenceError) r, Member (State Status) r, Member (State InferState) r) =>
    Module Shunted ->
    Sem r (Module Typed)
inferModule = traverseModuleRevTopologically @Shunted @Typed inferDeclaration

traverseExpr ::
    (Applicative f) =>
    (Located (Qualified Name) -> Typed.Expr -> f Typed.Expr) ->
    Typed.Declaration ->
    f Typed.Declaration
traverseExpr f =
    traverseOf
        (Typed._Declaration . unlocated . Typed._Declaration')
        ( \decl@(_, declName, _) -> traverseOf (_3 . unlocated . Typed._DeclarationBody . unlocated . Typed._Value) (f declName) decl
        )

inferDeclaration ::
    forall r.
    HasCallStack =>
    (Member (Error TypeInferenceError) r, Member (State Status) r, Member (State InferState) r) =>
    Shunted.Declaration ->
    Sem r Typed.Declaration
inferDeclaration (Shunted.Declaration ld) =
    Typed.Declaration
        <$> traverseOf
            unlocated
            ( \d' -> do
                let (Shunted.DeclarationBody ldb) = d' ^. unlocatedDeclarationBody

                db' <-
                    Typed.DeclarationBody
                        <$> traverseOf
                            unlocated
                            (inferDeclarationBody' (d' ^. name))
                            ldb
                pure (Typed.Declaration' (d' ^. moduleName) (d' ^. name) (db' `withLocationOf` ldb))
            )
            ld
  where
    inferDeclarationBody' ::
        HasCallStack =>
        Located (Qualified Name) ->
        Shunted.DeclarationBody' ->
        Sem r Typed.DeclarationBody'
    inferDeclarationBody' declName (Shunted.Value e maybeExpected) = do
        maybeExpected' <- for maybeExpected $
            \expected' -> do
                kind <- mapError KindInferError (inferTypeKind (expected' ^. unlocated))
                mapError KindInferError (unifyKinds kind TypeKind) -- expected type must be of kind Type
                astTypeToInferPolyType expected'

        -- add the expected type as an annotation
        -- this makes recursive definitions work (although perhaps it shouldn't)
        case maybeExpected' of
            Just expectedType -> push (Annotation (mkGlobal' declName) expectedType)
            Nothing -> pass

        e'@(Typed.Expr (_, ty)) <- inferExpression e maybeExpected'

        push (Annotation (mkGlobal' declName) ty)
        pure $ Typed.Value e'
    inferDeclarationBody' n (Shunted.TypeDeclaration tvs ty) = do
        ty' <-
            traverseOf
                unlocated
                ( \case
                    Renamed.Alias l -> do
                        inferType <- astTypeToInferType l
                        let vars' = createTypeVar <$> tvs
                        push (Annotation (mkGlobal' n) (Infer.Alias (generatedSourceRegionFrom n) (showPretty n) vars' inferType))
                        pure (Typed.Alias inferType)
                    Renamed.ADT constructors -> do
                        constructors' <- traverse (bitraverse pure (traverse astTypeToInferType)) constructors
                        let adtType = Infer.Custom (n ^. sourceRegion) (n ^. unlocated . to nameText) (createTypeVar <$> tvs)
                        traverse_ (\(c, b) -> addConstructorToContext tvs c b adtType) constructors'
                        pure $ Typed.ADT constructors'
                )
                ty
        kind <- mapError KindInferError (inferKind (fmap (^?! _NTypeName) (n ^. unlocated)) tvs (ty ^. unlocated))
        pure $ Typed.TypeDeclaration tvs ty' kind

addConstructorToContext :: (Member (State Status) r) => [Located (Unique LowerAlphaName)] -> Located (Qualified TypeName) -> [Infer.Type SourceRegion] -> Infer.Type SourceRegion -> Sem r ()
addConstructorToContext typeVars ctorName ctorArgs adtType = do
    let ctorType = foldr (\res acc -> Infer.Function (Infer.location acc) res acc) adtType ctorArgs
    -- type Option a = Some a | None
    -- Some : a -> Option a
    -- None : Option a
    let argsLoc = Infer.location <$> ctorArgs

    -- universally quantify the type over the type variables
    let forall' =
            foldr
                ( \(Located sr u) acc ->
                    Infer.Forall
                        (sconcat (ctorName ^. sourceRegion :| argsLoc))
                        sr
                        (showPretty u)
                        Domain.Type
                        acc
                )
                ctorType
                typeVars
    push (Annotation (mkGlobal' ctorName) forall')

createTypeVar :: Located (Unique LowerAlphaName) -> Infer.Type SourceRegion
createTypeVar (Located sr u) = Infer.VariableType sr (showPretty u)

freeTypeVars :: Located Renamed.Type -> [Located (Unique LowerAlphaName)]
freeTypeVars =
    nubOrdOn (view unlocated) -- remove duplicates, ignore location info when comparing
        . concatMapOf (cosmosOn unlocated) names
  where
    names :: Renamed.Type -> [Located (Unique LowerAlphaName)]
    names = \case
        Renamed.TypeVar l -> [l]
        _ -> [] -- cosmos takes care of the recursion :D

-- | Like 'astTypeToInferType' but universally quantifies over the free type variables
astTypeToInferPolyType :: (Member (State Status) r, Member (Error TypeInferenceError) r) => Located Renamed.Type -> Sem r (Infer.Type SourceRegion)
astTypeToInferPolyType l = universallyQuantify (freeTypeVars l) <$> astTypeToInferType l
  where
    universallyQuantify :: [Located (Unique LowerAlphaName)] -> Infer.Type SourceRegion -> Infer.Type SourceRegion
    universallyQuantify [] x = x
    universallyQuantify (Located sr u : us) t = Infer.Forall sr sr (fullTypeVarName u) Domain.Type (universallyQuantify us t)

fullTypeVarName :: Unique LowerAlphaName -> Text
fullTypeVarName = uniqueToText (^. _LowerAlphaName)

astTypeToInferType :: (Member (State Status) r, Member (Error TypeInferenceError) r) => Located Renamed.Type -> Sem r (Infer.Type SourceRegion)
astTypeToInferType (Located sr ut) = astTypeToInferType' ut
  where
    astTypeToInferType' ((Renamed.TypeVar l)) = pure (Infer.VariableType sr (l ^. unlocated . to fullTypeVarName)) -- todo: make this not rely on prettyShow
    astTypeToInferType' Renamed.UnitType = pure (Infer.Scalar sr Mono.Unit)
    astTypeToInferType' t@(Renamed.UserDefinedType n) = do
        ctx <- Infer.get
        case Context.lookup (mkGlobal' n) ctx of
            Just ty -> pure ty
            Nothing -> throw (UserDefinedTypeNotInContext sr t ctx)
    astTypeToInferType' ((Renamed.FunctionType a b)) = Infer.Function sr <$> astTypeToInferType a <*> astTypeToInferType b
    astTypeToInferType' ((Renamed.TupleType ts)) = Infer.Tuple sr <$> traverse astTypeToInferType ts
    astTypeToInferType' (Renamed.ListType ts) = Infer.List sr <$> astTypeToInferType ts
    astTypeToInferType' ((Renamed.TypeConstructorApplication ctor arg)) = do
        ctor' <- astTypeToInferType ctor
        arg' <- astTypeToInferType arg

        case ctor' of
            Infer.Custom{..} -> pure $ Infer.Custom location name (typeArguments ++ [arg'])
            Infer.Alias{..} -> pure $ Infer.Alias location name (typeArguments ++ [arg']) value
            other -> error (showColored other)
    astTypeToInferType' other = error (showColored other)

inferPattern ::
    forall r.
    (Member (Error TypeInferenceError) r, Member (State Status) r) =>
    Shunted.Pattern ->
    Maybe (Infer.Type SourceRegion) ->
    Sem r Typed.Pattern
inferPattern p@(Shunted.Pattern lp) expected = do
    (ty', p') <-
        Infer.inferPattern
            p
            (traverseOf unlocated inferPattern' lp)
    ctx <- Infer.get
    whenJust expected (checkPattern p) -- check that the inferred type is a subtype of the expected type
    let completedType = complete ctx (fromMaybe ty' expected)
    -- We set the type of the expression to the expected type if it was given, otherwise we use the inferred type

    pure $ Typed.Pattern (p', completedType)
  where
    inferPattern' :: Shunted.Pattern' -> Sem r Typed.Pattern'
    inferPattern' (Shunted.VarPattern v) = pure (Typed.VarPattern v)
    inferPattern' Shunted.WildcardPattern = pure Typed.WildcardPattern
    inferPattern' (Shunted.ListPattern l) = Typed.ListPattern <$> traverse (`inferPattern` Nothing) l
    inferPattern' (Shunted.ConsPattern l r) = Typed.ConsPattern <$> (`inferPattern` Nothing) l <*> (`inferPattern` Nothing) r
    inferPattern' other = error (showColored other)

inferExpression ::
    forall r.
    HasCallStack =>
    (Member (Error TypeInferenceError) r, Member (State Status) r) =>
    Shunted.Expr ->
    Maybe (Infer.Type SourceRegion) ->
    Sem r Typed.Expr
inferExpression e@(Shunted.Expr le) expected = do
    (ty', e') <-
        infer
            e
            (traverseOf unlocated inferExpression' le)
    ctx <- Infer.get
    whenJust expected (check e) -- check that the inferred type is a subtype of the expected type
    -- TODO: this is pretty bad for performance, every expression has to be checked twice. However just doing `subtype expected ty'` does not seem to work
    let completedType = complete ctx (fromMaybe ty' expected)
    {- We set the type of the expression to the expected type if it was given, otherwise we use the inferred type
    This avoids weird behaviour of a value's inferred type being more general than the actually specified one -}

    pure $ Typed.Expr (e', completedType)
  where
    inferExpression' :: Shunted.Expr' -> Sem r Typed.Expr'
    inferExpression' (Shunted.Int l) = pure $ Typed.Int l
    inferExpression' (Shunted.Float l) = pure $ Typed.Float l
    inferExpression' (Shunted.String l) = pure $ Typed.String l
    inferExpression' (Shunted.Char l) = pure $ Typed.Char l
    inferExpression' Shunted.Unit = pure Typed.Unit
    inferExpression' (Shunted.Var v) = pure (Typed.Var v)
    inferExpression' (Shunted.Constructor c) = pure (Typed.Constructor c)
    inferExpression' (Shunted.Lambda v body) = do
        e' <- inferExpression body Nothing
        pure $ Typed.Lambda v e'
    inferExpression' (Shunted.FunctionCall e1 e2) = do
        e1' <- inferExpression e1 Nothing
        e2' <- inferExpression e2 Nothing
        pure $ Typed.FunctionCall e1' e2'
    inferExpression' (Shunted.If e1 e2 e3) = do
        e1' <- inferExpression e1 Nothing
        e2' <- inferExpression e2 Nothing
        e3' <- inferExpression e3 Nothing
        pure $ Typed.If e1' e2' e3'
    inferExpression' (Shunted.List es) = do
        es' <- traverse (`inferExpression` Nothing) es
        pure $ Typed.List es'
    inferExpression' (Shunted.Match against ps) = do
        e'@(Typed.Expr (_, t)) <- inferExpression against Nothing

        ps' <- for ps $ \(pat, ok) -> do
            pat' <- inferPattern pat (Just t)
            ok' <- inferExpression ok Nothing
            pure (pat', ok')

        pure $ Typed.Match e' ps'
    inferExpression' (Shunted.LetIn n val body) = do
        val' <- inferExpression val Nothing
        body' <- inferExpression body Nothing
        pure $ Typed.LetIn n val' body'
    inferExpression' (Shunted.Let n val) = do
        val' <- inferExpression val Nothing
        pure $ Typed.Let n val'
    inferExpression' (Shunted.Block es) = do
        es' <- traverse (`inferExpression` Nothing) es
        pure $ Typed.Block es'
    inferExpression' (Shunted.Tuple ts) = do
        ts' <- traverse (`inferExpression` Nothing) ts
        pure $ Typed.Tuple ts'
