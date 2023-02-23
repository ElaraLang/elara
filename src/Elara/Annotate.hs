{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Elara.Annotate where

import Control.Lens (over, traverseOf, view, (^.))
import Data.Functor.Extra ((<<$), (<<<$>>>))
import Data.Map qualified as M
import Elara.AST.Annotated qualified as Annotated
import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Module (Declaration (..), Declaration' (Declaration'), DeclarationBody (..), DeclarationBody' (..), Exposing (..), Exposition (..), HasDeclarations (declarations), HasExposing (exposing), HasImports (imports), HasName (name), Import (..), Import' (Import'), Module (..), Module' (..), _Declaration, _DeclarationBody, _Import, _Module)
import Elara.AST.Module.Inspection (ContextBuildingError, InspectionContext, InspectionError, buildContext, search)
import Elara.AST.Name (MaybeQualified (MaybeQualified), ModuleName, Name (..), OpName, Qualified (Qualified), TypeName, VarName)
import Elara.AST.Region (Located (Located), SourceRegion, getLocation, unlocate, _Unlocate)
import Elara.AST.Select (Annotated, Frontend)
import Elara.Error (ReportableError (report))
import Polysemy (Member, Sem)
import Polysemy.Error (Error, runError, throw)
import Polysemy.Reader (Reader, ask, runReader)
import Prelude hiding (Reader, Type, ask, runReader)

type Modules = M.Map ModuleName (Module Frontend)

data AnnotationError
    = AnnotationError (InspectionError Frontend) SourceRegion
    | ContextBuildingError (ContextBuildingError Frontend)
    deriving (Show)

instance ReportableError AnnotationError where
    report (AnnotationError e _) = report e
    report (ContextBuildingError e) = report e
wrapError :: Member (Error AnnotationError) r => SourceRegion -> Sem (Error (InspectionError Frontend) : r) b -> Sem r b
wrapError region sem = do
    runError sem >>= \case
        Left err -> throw (AnnotationError err region)
        Right a -> pure a

wrapCtxError :: Member (Error AnnotationError) r => Sem (Error (ContextBuildingError Frontend) : r) b -> Sem r b
wrapCtxError sem = do
    runError sem >>= \case
        Left err -> throw (ContextBuildingError err)
        Right a -> pure a

annotateModule ::
    forall r.
    (Member (Reader Modules) r, Member (Error AnnotationError) r) =>
    Module Frontend ->
    Sem r (Module Annotated)
annotateModule m = traverseOf (_Module . _Unlocate) (const annotate) m
  where
    annotate :: Sem r (Module' Annotated)
    annotate = do
        modules <- ask
        context <- wrapCtxError $ buildContext m modules
        exposing' <- runReader context $ annotateExposing (m ^. exposing)
        imports' <- runReader context (traverse annotateImport (m ^. imports))
        declarations' <- runReader context (traverse annotateDeclaration (m ^. declarations))
        pure (Module' (m ^. name) exposing' imports' declarations')

    annotateExposing :: Exposing Frontend -> Sem (Reader InspectionContext : r) (Exposing Annotated)
    annotateExposing ExposingAll = pure ExposingAll
    annotateExposing (ExposingSome expos) = ExposingSome <$> traverse annotateExposition expos

    annotateExposition :: Exposition Frontend -> Sem (Reader InspectionContext : r) (Exposition Annotated)
    annotateExposition (ExposedValue name') = ExposedValue <$> annotateVarName name'
    annotateExposition (ExposedOp name') = ExposedOp <$> annotateOpName name'
    annotateExposition (ExposedType name') = ExposedType <$> annotateTypeName name'
    annotateExposition (ExposedTypeAndAllConstructors name') = ExposedTypeAndAllConstructors <$> annotateTypeName name'

    annotateVarName :: Located (MaybeQualified VarName) -> Sem (Reader InspectionContext : r) (Located (Qualified VarName))
    annotateVarName nv@(Located loc mq@(MaybeQualified name' mMod)) = case mMod of
        Just qual -> pure (nv $> Qualified name' qual)
        Nothing -> do
            q <- wrapError loc $ search (NVarName <$> mq)
            pure (nv $> Qualified name' q)

    annotateTypeName :: Located (MaybeQualified TypeName) -> Sem (Reader InspectionContext : r) (Located (Qualified TypeName))
    annotateTypeName nv@(Located loc mq@(MaybeQualified name' mMod)) = case mMod of
        Just qual -> pure (nv $> Qualified name' qual)
        Nothing -> do
            q <- wrapError loc $ search (NTypeName <$> mq)
            pure (nv $> Qualified name' q)

    annotateOpName :: Located (MaybeQualified OpName) -> Sem (Reader InspectionContext : r) (Located (Qualified OpName))
    annotateOpName nv@(Located loc mq@(MaybeQualified name' mMod)) = case mMod of
        Just qual -> pure (nv $> Qualified name' qual)
        Nothing -> do
            q <- wrapError loc $ search (NOpName <$> mq)
            pure (nv $> Qualified name' q)

    annotateName :: Located (MaybeQualified Name) -> Sem (Reader InspectionContext : r) (Located (Qualified Name))
    annotateName nv@(Located _ (MaybeQualified (NVarName name') _)) = NVarName <<<$>>> annotateVarName (name' <<$ nv)
    annotateName nv@(Located _ (MaybeQualified (NTypeName name') _)) = NTypeName <<<$>>> annotateTypeName (name' <<$ nv)
    annotateName nv@(Located _ (MaybeQualified (NOpName name') _)) = NOpName <<<$>>> annotateOpName (name' <<$ nv)

    annotateImport :: Import Frontend -> Sem (Reader InspectionContext : r) (Import Annotated)
    annotateImport = traverseOf (_Import . _Unlocate) (\(Import' name' as' qualified' exposing') -> Import' name' as' qualified' <$> annotateExposing exposing')

    annotateDeclaration :: Declaration Frontend -> Sem (Reader InspectionContext : r) (Declaration Annotated)
    annotateDeclaration =
        traverseOf
            (_Declaration . _Unlocate)
            ( \(Declaration' module' name' body') -> do
                annotatedName <- annotateName name'
                annotatedBody <- annotateDeclarationBody body'
                pure (Declaration' module' annotatedName annotatedBody)
            )

    annotateDeclarationBody :: DeclarationBody Frontend -> Sem (Reader InspectionContext : r) (DeclarationBody Annotated)
    annotateDeclarationBody = traverseOf (_DeclarationBody . _Unlocate) annotateDeclarationBody'

    annotateDeclarationBody' (Value expr patterns typeAnnotation) = do
        annotatedExpr <- annotateExpr expr
        annotatedPatterns <- traverse annotatePattern patterns
        annotatedTypeAnnotation <- traverse annotateType (typeAnnotation :: Maybe Frontend.TypeAnnotation)

        pure (Value annotatedExpr annotatedPatterns annotatedTypeAnnotation)
    annotateDeclarationBody' (ValueTypeDef type') = do
        annotatedType <- traverseOf _Unlocate (traverse annotateType) type'
        pure (ValueTypeDef annotatedType)
    annotateDeclarationBody' (TypeAlias type') = do
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
    annotateExpr' (Frontend.InParens expr) = Annotated.InParens <$> annotateExpr expr

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

    -- annotateOp :: Frontend.BinaryOperator -> Sem (Reader InspectionContext : r) Annotated.BinaryOperator
    annotateOp (Frontend.MkBinaryOperator o) = Annotated.MkBinaryOperator <$> traverse annotateOp' o
    annotateOp' (Frontend.Op o) = Annotated.Op <$> annotateOpName o
    annotateOp' (Frontend.Infixed o) = Annotated.Infixed <$> annotateVarName o

    annotateType :: Frontend.TypeAnnotation -> Sem (Reader InspectionContext : r) Annotated.TypeAnnotation
    annotateType (Frontend.TypeAnnotation annotationName type') = Annotated.TypeAnnotation <$> annotateName annotationName <*> annotateType' type'

    annotateType' :: Frontend.Type -> Sem (Reader InspectionContext : r) Annotated.Type
    annotateType' (Frontend.TypeVar tv) = pure (Annotated.TypeVar tv)
    annotateType' (Frontend.FunctionType t1 t2) = Annotated.FunctionType <$> annotateType' t1 <*> annotateType' t2
    annotateType' Frontend.UnitType = pure Annotated.UnitType
    annotateType' (Frontend.TypeConstructorApplication ctor arg) = Annotated.TypeConstructorApplication <$> annotateType' ctor <*> annotateType' arg
    annotateType' (Frontend.UserDefinedType tn) = Annotated.UserDefinedType <$> annotateTypeName tn

unfoldLambda :: [Annotated.Pattern] -> Annotated.Expr -> Annotated.Expr
unfoldLambda [] expr = expr
unfoldLambda (p : ps) expr =
    over
        Annotated._Expr
        (\expr' -> Located (getLocation expr') (Annotated.Lambda p (unfoldLambda ps expr)))
        expr