module Elara.TypeInfer where

import Control.Lens (traverseOf, (^.), _3)
import Elara.AST.Module
import Elara.AST.Name (Name, Qualified, _LowerAlphaName)
import Elara.AST.Region (Located (Located), SourceRegion, generatedSourceRegionFrom, unlocated)
import Elara.AST.Renamed qualified as Renamed
import Elara.AST.Select
import Elara.AST.Shunted as Shunted hiding (Type)
import Elara.AST.Shunted qualified as Shunted
import Elara.AST.Typed as Typed
import Elara.AST.VarRef (mkGlobal')
import Elara.Data.Unique (uniqueVal)
import Elara.TypeInfer.Context
import Elara.TypeInfer.Error (TypeInferenceError)
import Elara.TypeInfer.Infer hiding (get)
import Elara.TypeInfer.Infer qualified as Infer
import Elara.TypeInfer.Monotype qualified as Mono
import Elara.TypeInfer.Type qualified as Infer
import Polysemy hiding (transform)
import Polysemy.Error (Error)
import Polysemy.State
import Print (prettyShow)
import TODO (todo)

inferModule ::
    forall r.
    (Member (Error TypeInferenceError) r, Member (State Status) r) =>
    Module Shunted ->
    Sem r (Module Typed)
inferModule m = do
    traverseModule @Shunted @Typed inferDeclaration m

traverseExpr :: (Applicative f) => (Located (Qualified Name) -> Typed.Expr -> f Typed.Expr) -> Typed.Declaration -> f Typed.Declaration
traverseExpr f =
    traverseOf
        (Typed._Declaration . unlocated . Typed._Declaration')
        ( \decl@(_, declName, _) -> do
            traverseOf (_3 . Typed._DeclarationBody . unlocated . Typed._Value) (f declName) decl
        )

inferDeclaration ::
    forall r.
    (Member (Error TypeInferenceError) r, Member (State Status) r) =>
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
        maybeExpected' <- traverse astTypeToInferType maybeExpected

        -- add the expected type as an annotation
        -- this makes recursive definitions work (although perhaps it shouldn't)
        case maybeExpected' of
            Just expectedType -> push (Annotation (mkGlobal' declName) expectedType)
            Nothing -> fresh >>= push . Annotation (mkGlobal' declName) . Infer.UnsolvedType (generatedSourceRegionFrom declName)

        e'@(Typed.Expr (_, ty)) <- inferExpression e

        whenJust maybeExpected' $ \expected' -> do
            subtype ty expected' -- make sure the inferred type is a subtype of the expected type
        push (Annotation (mkGlobal' declName) ty)
        pure $ Typed.Value e'
    inferDeclarationBody' _ (Shunted.TypeDeclaration vs ty) = do
        ty' <-
            traverseOf
                unlocated
                ( \case
                    Renamed.Alias l -> Typed.Alias <$> astTypeToInferType l
                    Renamed.ADT constructors -> do
                        constructors' <- traverse (bitraverse pure (traverse astTypeToInferType)) constructors
                        pure $ Typed.ADT constructors'
                )
                ty
        pure $ Typed.TypeDeclaration vs ty'

astTypeToInferType :: Located Renamed.Type -> Sem r (Infer.Type SourceRegion)
astTypeToInferType (Located sr (Renamed.TypeVar l)) = pure (Infer.VariableType sr (l ^. uniqueVal . _LowerAlphaName))
astTypeToInferType (Located sr Renamed.UnitType) = pure (Infer.Scalar sr Mono.Unit)
astTypeToInferType _ = todo

inferExpression ::
    forall r.
    (Member (Error TypeInferenceError) r, Member (State Status) r) =>
    Shunted.Expr ->
    Sem r Typed.Expr
inferExpression e@(Shunted.Expr le) = do
    (ty', e') <-
        infer
            e
            (traverseOf unlocated (subsume_ . inferExpression') le)
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
