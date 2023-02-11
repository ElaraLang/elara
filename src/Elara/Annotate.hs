{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Elara.Annotate where

import Control.Lens (over, view, (^.))
import Data.Map qualified as M
import Elara.AST.Annotated qualified as Annotated
import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Module (Declaration (..), DeclarationBody (..), Exposing (..), Exposition (..), HasDeclarations (declarations), HasExposing (exposing), HasImports (imports), HasName (name), Import (..), Module (..))
import Elara.AST.Module.Inspection (InspectionContext, InspectionError, buildContext, search)
import Elara.AST.Name (MaybeQualified (MaybeQualified), ModuleName, Name (..), OpName, Qualified (Qualified), TypeName, VarName)
import Elara.AST.Region (Located (Located), getLocation, unlocate)
import Elara.AST.Select (Annotated, Frontend)
import Elara.Data.Type (Type (..))
import Polysemy (Member, Sem)
import Polysemy.Error (Error)
import Polysemy.Reader (Reader, ask, runReader)
import Prelude hiding (Reader, Type, ask, runReader)

type Modules = M.Map ModuleName (Module Frontend)

annotateModule ::
    forall r.
    (Member (Reader Modules) r, Member (Error (InspectionError Frontend)) r) =>
    Module Frontend ->
    Sem r (Module Annotated)
annotateModule m = do
    modules <- ask
    let context = buildContext m modules
    exposing' <- runReader context $ annotateExposing (m ^. exposing)
    imports' <- runReader context (traverse annotateImport (m ^. imports))
    declarations' <- runReader context (traverse annotateDeclaration (m ^. declarations))
    pure (Module (m ^. name) exposing' imports' declarations')
  where
    annotateExposing :: Exposing MaybeQualified -> Sem (Reader InspectionContext : r) (Exposing Qualified)
    annotateExposing ExposingAll = pure ExposingAll
    annotateExposing (ExposingSome expos) = ExposingSome <$> traverse annotateExposition expos

    annotateExposition :: Exposition MaybeQualified -> Sem (Reader InspectionContext : r) (Exposition Qualified)
    annotateExposition (ExposedValue name') = ExposedValue <$> annotateVarName name'
    annotateExposition (ExposedType name') = ExposedType <$> annotateTypeName name'
    annotateExposition (ExposedTypeAndAllConstructors name') = ExposedTypeAndAllConstructors <$> annotateTypeName name'

    annotateVarName :: MaybeQualified VarName -> Sem (Reader InspectionContext : r) (Qualified VarName)
    annotateVarName nv@(MaybeQualified name' mMod) = case mMod of
        Just qual -> pure (Qualified name' qual)
        Nothing -> do
            q <- search (NVarName nv)
            pure (Qualified name' q)

    annotateTypeName :: MaybeQualified TypeName -> Sem (Reader InspectionContext : r) (Qualified TypeName)
    annotateTypeName nv@(MaybeQualified name' mMod) = case mMod of
        Just qual -> pure (Qualified name' qual)
        Nothing -> do
            q <- search (NTypeName nv)
            pure (Qualified name' q)

    annotateOpName :: MaybeQualified OpName -> Sem (Reader InspectionContext : r) (Qualified OpName)
    annotateOpName nv@(MaybeQualified name' mMod) = case mMod of
        Just qual -> pure (Qualified name' qual)
        Nothing -> do
            q <- search (NOpName nv)
            pure (Qualified name' q)

    annotateName :: Name MaybeQualified -> Sem (Reader InspectionContext : r) (Name Qualified)
    annotateName (NVarName name') = NVarName <$> annotateVarName name'
    annotateName (NTypeName name') = NTypeName <$> annotateTypeName name'
    annotateName (NOpName name') = NOpName <$> annotateOpName name'

    annotateImport :: Import MaybeQualified -> Sem (Reader InspectionContext : r) (Import Qualified)
    annotateImport (Import name' as' qualified' exposing') = Import name' as' qualified' <$> annotateExposing exposing'

    annotateDeclaration :: Declaration Frontend -> Sem (Reader InspectionContext : r) (Declaration Annotated)
    annotateDeclaration (Declaration module' name' body') = do
        annotatedName <- annotateName name'
        annotatedBody <- annotateDeclarationBody body'
        pure (Declaration module' annotatedName annotatedBody)

    annotateDeclarationBody :: DeclarationBody Frontend -> Sem (Reader InspectionContext : r) (DeclarationBody Annotated)
    annotateDeclarationBody (Value expr patterns typeAnnotation) = do
        annotatedExpr <- annotateExpr expr
        annotatedPatterns <- traverse annotatePattern patterns
        annotatedTypeAnnotation <- traverse annotateType typeAnnotation
        pure (Value annotatedExpr annotatedPatterns annotatedTypeAnnotation)
    annotateDeclarationBody (ValueTypeDef type') = do
        annotatedType <- traverse annotateType type'
        pure (ValueTypeDef annotatedType)
    annotateDeclarationBody (TypeAlias type') = do
        annotatedType <- annotateType' type'
        pure (TypeAlias annotatedType)

    annotateExpr :: Frontend.Expr -> Sem (Reader InspectionContext : r) Annotated.Expr
    annotateExpr (Frontend.Expr expr') = Annotated.Expr <$> traverse annotateExpr' expr'

    annotateExpr' (Frontend.Int i) = pure (Annotated.Int i)
    annotateExpr' (Frontend.Float f) = pure (Annotated.Float f)
    annotateExpr' (Frontend.String s) = pure (Annotated.String s)
    annotateExpr' (Frontend.Char c) = pure (Annotated.Char c)
    annotateExpr' Frontend.Unit = pure Annotated.Unit
    annotateExpr' (Frontend.Var name') = do
        annotatedName <- annotateVarName name'
        pure (Annotated.Var annotatedName)
    annotateExpr' (Frontend.Constructor name') = do
        annotatedName <- annotateTypeName name'
        pure (Annotated.Constructor annotatedName)
    annotateExpr' (Frontend.Lambda patterns expr) = do
        annotatedPatterns <- traverse annotatePattern patterns
        annotatedExpr <- annotateExpr expr
        let lambda = unfoldLambda annotatedPatterns annotatedExpr
        pure (unlocate (view Annotated._Expr lambda))
    annotateExpr' (Frontend.FunctionCall f x) = Annotated.FunctionCall <$> annotateExpr f <*> annotateExpr x
    annotateExpr' (Frontend.If cond then' else') = do
        annotatedCond <- annotateExpr cond
        annotatedThen <- annotateExpr then'
        annotatedElse <- annotateExpr else'
        pure (Annotated.If annotatedCond annotatedThen annotatedElse)
    annotateExpr' (Frontend.BinaryOperator op left right) = do
        annotatedOp <- annotateOp op
        annotatedLeft <- annotateExpr left
        annotatedRight <- annotateExpr right
        pure (Annotated.BinaryOperator annotatedOp annotatedLeft annotatedRight)
    annotateExpr' (Frontend.List xs) = Annotated.List <$> traverse annotateExpr xs
    annotateExpr' (Frontend.LetIn lName lPats lExp lBody) = do
        annotatedName <- annotateVarName lName
        annotatedPats <- traverse annotatePattern lPats
        annotatedExp <- annotateExpr lExp
        annotatedBody <- annotateExpr lBody
        let lambdaExp = unfoldLambda annotatedPats annotatedExp
        pure (Annotated.LetIn annotatedName lambdaExp annotatedBody)
    annotateExpr' (Frontend.Let lName lPats lExp) = do
        annotatedName <- annotateVarName lName
        annotatedPats <- traverse annotatePattern lPats
        annotatedExp <- annotateExpr lExp
        let lambdaExp = unfoldLambda annotatedPats annotatedExp
        pure (Annotated.Let annotatedName lambdaExp)
    annotateExpr' (Frontend.Block exprs) = do
        annotatedExprs <- traverse annotateExpr exprs
        pure (Annotated.Block annotatedExprs)

    annotatePattern :: Frontend.Pattern -> Sem (Reader InspectionContext : r) Annotated.Pattern
    annotatePattern (Frontend.Pattern pattern') = Annotated.Pattern <$> traverse annotatePattern' pattern'

    annotatePattern' (Frontend.NamedPattern name') = do
        pure (Annotated.NamedPattern name')
    annotatePattern' (Frontend.ConstructorPattern constructorName patterns) = do
        annotatedConstructorName <- annotateTypeName constructorName
        annotatedPatterns <- traverse annotatePattern patterns
        pure (Annotated.ConstructorPattern annotatedConstructorName annotatedPatterns)
    annotatePattern' (Frontend.ListPattern i) = Annotated.ListPattern <$> traverse annotatePattern i
    annotatePattern' Frontend.WildcardPattern = pure Annotated.WildcardPattern

    annotateOp :: Frontend.BinaryOperator -> Sem (Reader InspectionContext : r) Annotated.BinaryOperator
    annotateOp (Frontend.MkBinaryOperator o) = Annotated.MkBinaryOperator <$> traverse annotateOp' o
    annotateOp' (Frontend.Op o) = Annotated.Op <$> annotateOpName o
    annotateOp' (Frontend.Infixed o) = Annotated.Infixed <$> annotateVarName o

    annotateType :: Frontend.TypeAnnotation -> Sem (Reader InspectionContext : r) Annotated.TypeAnnotation
    annotateType (Frontend.TypeAnnotation annotationName type') = Annotated.TypeAnnotation <$> annotateName annotationName <*> annotateType' type'

    annotateType' :: Type MaybeQualified -> Sem (Reader InspectionContext : r) (Type Qualified)
    annotateType' (TypeVar tv) = pure (TypeVar tv)
    annotateType' (FunctionType t1 t2) = FunctionType <$> annotateType' t1 <*> annotateType' t2
    annotateType' UnitType = pure UnitType
    annotateType' (TypeConstructorApplication ctor arg) = TypeConstructorApplication <$> annotateType' ctor <*> annotateType' arg
    annotateType' (UserDefinedType tn) = UserDefinedType <$> annotateTypeName tn

unfoldLambda :: [Annotated.Pattern] -> Annotated.Expr -> Annotated.Expr
unfoldLambda [] expr = expr
unfoldLambda (p : ps) expr =
    over
        Annotated._Expr
        (\expr' -> Located (getLocation expr') (Annotated.Lambda p (unfoldLambda ps expr)))
        expr