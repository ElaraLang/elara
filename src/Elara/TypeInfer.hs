{-# LANGUAGE RecordWildCards #-}

module Elara.TypeInfer where

import Control.Lens (to, traverseOf, (^.), (^?!), _3)
import Data.Traversable (for)
import Elara.AST.Module
import Elara.AST.Name (LowerAlphaName, Name, Qualified, TypeName, nameText, _NTypeName)
import Elara.AST.Region (Located (Located), SourceRegion, generatedSourceRegionFrom, sourceRegion, unlocated)
import Elara.AST.Renamed qualified as Renamed
import Elara.AST.Select
import Elara.AST.Shunted as Shunted
import Elara.AST.Typed as Typed
import Elara.AST.VarRef (mkGlobal')
import Elara.Data.Kind (ElaraKind (TypeKind))
import Elara.Data.Kind.Infer (InferState, inferKind, inferTypeKind, unifyKinds)
import Elara.Data.Unique (Unique)
import Elara.TypeInfer.Context
import Elara.TypeInfer.Context qualified as Context
import Elara.TypeInfer.Domain qualified as Domain
import Elara.TypeInfer.Error (TypeInferenceError (KindInferError, UserDefinedTypeNotInContext))
import Elara.TypeInfer.Infer hiding (get)
import Elara.TypeInfer.Infer qualified as Infer
import Elara.TypeInfer.Monotype qualified as Mono
import Elara.TypeInfer.Type qualified as Infer
import Polysemy hiding (transform)
import Polysemy.Error (Error, mapError, throw)
import Polysemy.State
import Print
import TODO (todo)

inferModule ::
    forall r.
    (Member (Error TypeInferenceError) r, Member (State Status) r, Member (State InferState) r) =>
    Module Shunted ->
    Sem r (Module Typed)
inferModule = traverseModuleRevTopologically @Shunted @Typed inferDeclaration

traverseExpr :: (Applicative f) => (Located (Qualified Name) -> Typed.Expr -> f Typed.Expr) -> Typed.Declaration -> f Typed.Declaration
traverseExpr f =
    traverseOf
        (Typed._Declaration . unlocated . Typed._Declaration')
        ( \decl@(_, declName, _) -> traverseOf (_3 . Typed._DeclarationBody . unlocated . Typed._Value) (f declName) decl
        )

inferDeclaration ::
    forall r.
    (Member (Error TypeInferenceError) r, Member (State Status) r, Member (State InferState) r) =>
    Shunted.Declaration ->
    Sem r Typed.Declaration
inferDeclaration (Shunted.Declaration ld) =
    Typed.Declaration
        <$> traverseOf
            unlocated
            ( \d' -> do
                let (Shunted.DeclarationBody ldb) = d' ^. Shunted.declaration'Body
                db' <-
                    Typed.DeclarationBody
                        <$> traverseOf
                            unlocated
                            (inferDeclarationBody' (d' ^. name))
                            ldb
                pure (Typed.Declaration' (d' ^. moduleName) (d' ^. name) db')
            )
            ld
  where
    inferDeclarationBody' ::
        Located (Qualified Name) ->
        Shunted.DeclarationBody' ->
        Sem r Typed.DeclarationBody'
    inferDeclarationBody' declName (Shunted.Value e maybeExpected) = do
        maybeExpected' <- for maybeExpected $
            \expected' -> do
                kind <- mapError KindInferError (inferTypeKind (expected' ^. unlocated))
                mapError KindInferError (unifyKinds kind TypeKind) -- expected type must be of kind Type
                astTypeToInferType expected'

        -- add the expected type as an annotation
        -- this makes recursive definitions work (although perhaps it shouldn't)
        case maybeExpected' of
            Just expectedType -> push (Annotation (mkGlobal' declName) expectedType)
            Nothing -> pass

        e'@(Typed.Expr (_, ty)) <- inferExpression e

        whenJust maybeExpected' $ \expected' -> subtype ty expected' -- make sure the inferred type is a subtype of the expected type
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
    -- debugColored ("adding constructor", ctorName)
    push (Annotation (mkGlobal' ctorName) forall')

createTypeVar :: Located (Unique LowerAlphaName) -> Infer.Type SourceRegion
createTypeVar (Located sr u) = Infer.VariableType sr (showPretty u)

astTypeToInferType :: (Member (State Status) r, Member (Error TypeInferenceError) r) => Located Renamed.Type -> Sem r (Infer.Type SourceRegion)
astTypeToInferType (Located sr (Renamed.TypeVar l)) = pure (Infer.VariableType sr (showPretty l)) -- todo: make this not rely on prettyShow
astTypeToInferType (Located sr Renamed.UnitType) = pure (Infer.Scalar sr Mono.Unit)
astTypeToInferType (Located sr t@(Renamed.UserDefinedType n)) = do
    ctx <- Infer.get
    case Context.lookup (mkGlobal' n) ctx of
        Just ty -> pure ty
        Nothing -> throw (UserDefinedTypeNotInContext sr t ctx)
astTypeToInferType (Located sr (Renamed.FunctionType a b)) = Infer.Function sr <$> astTypeToInferType a <*> astTypeToInferType b
astTypeToInferType (Located sr (Renamed.TupleType ts)) = Infer.Tuple sr <$> traverse astTypeToInferType ts
astTypeToInferType (Located sr (Renamed.TypeConstructorApplication ctor arg)) = do
    ctor' <- astTypeToInferType ctor
    arg' <- astTypeToInferType arg

    case ctor' of
        Infer.Custom{..} -> pure $ Infer.Custom location name (typeArguments ++ [arg'])
        Infer.Alias{..} -> pure $ Infer.Alias location name (typeArguments ++ [arg']) value
        other -> error (showColored other)
astTypeToInferType other = error (showColored other)

inferExpression ::
    forall r.
    (Member (Error TypeInferenceError) r, Member (State Status) r) =>
    Shunted.Expr ->
    Sem r Typed.Expr
inferExpression e@(Shunted.Expr le) = do
    (ty', e') <-
        infer
            e
            (traverseOf unlocated inferExpression' le)
    ctx <- Infer.get
    pure $ Typed.Expr (e', complete ctx ty')
  where
    inferExpression' :: Shunted.Expr' -> Sem r Typed.Expr'
    inferExpression' (Shunted.Int l) = pure $ Typed.Int l
    inferExpression' (Shunted.Float l) = pure $ Typed.Float l
    inferExpression' (Shunted.String l) = pure $ Typed.String l
    inferExpression' (Shunted.Char l) = pure $ Typed.Char l
    inferExpression' Shunted.Unit = pure Typed.Unit
    inferExpression' (Shunted.Var v) = pure (Typed.Var v)
    inferExpression' (Shunted.Constructor c) = pure (Typed.Constructor c)
    inferExpression' (Shunted.Lambda v e) = do
        e' <- inferExpression e
        pure $ Typed.Lambda v e'
    inferExpression' (Shunted.FunctionCall e1 e2) = do
        e1' <- inferExpression e1
        e2' <- inferExpression e2
        pure $ Typed.FunctionCall e1' e2'
    inferExpression' (Shunted.If e1 e2 e3) = do
        e1' <- inferExpression e1
        e2' <- inferExpression e2
        e3' <- inferExpression e3
        pure $ Typed.If e1' e2' e3'
    inferExpression' (Shunted.List es) = do
        es' <- traverse inferExpression es
        pure $ Typed.List es'
    inferExpression' (Shunted.Match e ps) = todo
    inferExpression' (Shunted.LetIn n val body) = do
        val' <- inferExpression val
        body' <- inferExpression body
        pure $ Typed.LetIn n val' body'
    inferExpression' (Shunted.Let n val) = do
        val' <- inferExpression val
        pure $ Typed.Let n val'
    inferExpression' (Shunted.Block es) = do
        es' <- traverse inferExpression es
        pure $ Typed.Block es'
    inferExpression' (Shunted.Tuple ts) = do
        ts' <- traverse inferExpression ts
        pure $ Typed.Tuple ts'
