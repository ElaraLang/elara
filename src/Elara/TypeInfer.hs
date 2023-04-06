module Elara.TypeInfer where

import Control.Lens (traverseOf, traverseOf_, (^.), _1, _2, _3)
import Elara.AST.Module
import Elara.AST.Name (Name, Qualified)
import Elara.AST.Region (Located, SourceRegion, sourceRegion, unlocated)
import Elara.AST.Select
import Elara.AST.Shunted as Shunted hiding (Type)
import Elara.AST.Typed as Typed
import Elara.Data.Pretty
import Elara.TypeInfer.Context
import Elara.TypeInfer.Error (TypeInferenceError)
import Elara.TypeInfer.Infer hiding (get)
import Elara.TypeInfer.Infer qualified as Infer
import Elara.TypeInfer.Type hiding (name)
import Polysemy hiding (transform)
import Polysemy.Error (Error)
import Polysemy.State
import Print (debugColored, debugPretty)
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
        ( \decl@(_, name, _) -> do
            traverseOf (_3 . Typed._DeclarationBody . unlocated . Typed._Value) (f name) decl
        )

addStubsToModule :: (Member (State Status) r) => Shunted.Declaration -> Sem r ()
addStubsToModule = traverseOf_ (Shunted._Declaration . unlocated . Shunted._Declaration') addStubsToDeclaration
  where
    addStubsToDeclaration (_, n, body) =
        traverseOf_ (Shunted._DeclarationBody . unlocated . Shunted._Value . _2) (addValueDeclarationStub n) body

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
                db' <-
                    let (Shunted.DeclarationBody ldb) = d' ^. Shunted.declaration'Body
                     in Typed.DeclarationBody
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
    inferDeclarationBody' name (Shunted.Value e _) = do
        e'@(Typed.Expr (_, ty)) <- inferExpression e
        push (Annotation (mkGlobal' name) ty)
        pure $ Typed.Value e'
    inferDeclarationBody' _ (Shunted.TypeAlias ty) = error (show ty)

addValueDeclarationStub ::
    forall r.
    (Member (State Status) r) =>
    Located (Qualified Name) ->
    Maybe (Located Shunted.TypeAnnotation) ->
    Sem r (Type SourceRegion)
addValueDeclarationStub name ty = do
    expectedType <- case ty of
        Just x -> undefined
        Nothing -> Elara.TypeInfer.Type.UnsolvedType (name ^. sourceRegion) <$> fresh
    push (Annotation (mkGlobal' name) expectedType)
    pure expectedType

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
    inferExpression' (Shunted.Var v) = pure $ Typed.Var (fmap convertVarRef v)
    inferExpression' (Shunted.Constructor c) = pure $ Typed.Constructor (fmap convertVarRef c)
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

convertVarRef :: Shunted.VarRef n -> Typed.VarRef n
convertVarRef (Shunted.Global c) = Typed.Global c
convertVarRef (Shunted.Local c) = Typed.Local c
