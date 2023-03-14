{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Elara.Annotate where

import Control.Lens (over, traverseOf, view, (^.), _2)
import Data.Map qualified as M
import Elara.AST.Annotated qualified as Annotated
import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Module (Declaration (..), Declaration' (Declaration'), DeclarationBody (..), DeclarationBody' (..), Exposing (..), Exposition (..), HasDeclarations (declarations), HasExposing (exposing), HasImports (imports), HasName (name), Import (..), Import' (Import'), Module (..), Module' (..), _Declaration, _DeclarationBody, _Import, _Module)
import Elara.AST.Module.Inspection (ContextBuildingError, InspectionContext, InspectionError, buildContext, search)
import Elara.AST.Name (MaybeQualified (MaybeQualified), ModuleName, Name (..), NameLike (fullNameText), OpName, Qualified (Qualified), ToName (toName), TypeName, VarName)
import Elara.AST.Region (Located (Located), SourceRegion, sourceRegion, sourceRegionToDiagnosePosition, unlocated)
import Elara.AST.Select (Annotated, Frontend)
import Elara.Error (ReportableError (report), addPosition)
import Elara.Error.Codes qualified as Codes
import Error.Diagnose
import Polysemy (Member, Sem)
import Polysemy.Error (Error, runError, throw)
import Polysemy.Reader (Reader, ask, runReader)
import Prelude hiding (Reader, Type, ask, runReader)

type Modules = M.Map ModuleName (Module Frontend)

data AnnotationError
    = AnnotationError (InspectionError Frontend) SourceRegion
    | ContextBuildingError (ContextBuildingError Frontend)
    | QualifiedWithWrongModule (Located (Qualified Name)) ModuleName
    deriving (Show)

instance ReportableError AnnotationError where
    report (AnnotationError e sr) = do
        let r = report e
        let pos = sourceRegionToDiagnosePosition sr
        addPosition (pos, This "") r
    report (ContextBuildingError e) = report e
    report (QualifiedWithWrongModule (Located sr (Qualified n qual)) modName) = do
        let pos = sourceRegionToDiagnosePosition sr
        Err
            (Just Codes.qualifiedWithWrongModule)
            ("Qualification of name doesn't match module name: " <> fullNameText modName)
            [(pos, This "")]
            [ Note "If you choose to qualify your names, the qualification has to match the name of your module - you can't just pick any name."
            , Hint $ "Try changing " <> fullNameText qual <> " to " <> fullNameText modName
            , Hint $ "Try changing " <> fullNameText qual <> "." <> fullNameText n <> " to " <> fullNameText n
            , Hint $ "Try renaming your module to " <> fullNameText qual
            ]

wrapError :: Member (Error AnnotationError) r => SourceRegion -> Sem (Error (InspectionError Frontend) : r) b -> Sem r b
wrapError region sem = do
    runError sem >>= \case
        Left e -> throw (AnnotationError e region)
        Right a -> pure a

wrapCtxError :: Member (Error AnnotationError) r => Sem (Error (ContextBuildingError Frontend) : r) b -> Sem r b
wrapCtxError sem = do
    runError sem >>= \case
        Left e -> throw (ContextBuildingError e)
        Right a -> pure a

annotateModule ::
    forall r.
    (Member (Reader Modules) r, Member (Error AnnotationError) r) =>
    Module Frontend ->
    Sem r (Module Annotated)
annotateModule thisModule = traverseOf (_Module . unlocated) (const annotate) thisModule
  where
    annotate :: Sem r (Module' Annotated)
    annotate = do
        modules <- ask
        context <- wrapCtxError $ buildContext thisModule modules
        exposing' <- runReader context $ annotateExposing (thisModule ^. exposing)
        imports' <- runReader context (traverse annotateImport (thisModule ^. imports))
        declarations' <- runReader context (traverse annotateDeclaration (thisModule ^. declarations))
        pure (Module' (thisModule ^. name) exposing' imports' declarations')

    annotateExposing :: Exposing Frontend -> Sem (Reader InspectionContext : r) (Exposing Annotated)
    annotateExposing ExposingAll = pure ExposingAll
    annotateExposing (ExposingSome expos) = ExposingSome <$> traverse annotateExposition expos

    annotateExposition :: Exposition Frontend -> Sem (Reader InspectionContext : r) (Exposition Annotated)
    annotateExposition (ExposedValue name') = ExposedValue <$> annotateVarName name'
    annotateExposition (ExposedOp name') = ExposedOp <$> annotateOpName name'
    annotateExposition (ExposedType name') = ExposedType <$> annotateTypeName name'
    annotateExposition (ExposedTypeAndAllConstructors name') = ExposedTypeAndAllConstructors <$> annotateTypeName name'

    annotateGenericName ctor nv@(Located loc mq@(MaybeQualified name' mMod)) = case mMod of
        Just qual -> pure (nv $> Qualified name' qual)
        Nothing -> do
            q <- wrapError loc $ search (ctor <$> mq)
            pure (nv $> Qualified name' q)

    annotateVarName :: Located (MaybeQualified VarName) -> Sem (Reader InspectionContext : r) (Located (Qualified VarName))
    annotateVarName = annotateGenericName NVarName
    annotateTypeName :: Located (MaybeQualified TypeName) -> Sem (Reader InspectionContext : r) (Located (Qualified TypeName))
    annotateTypeName = annotateGenericName NTypeName
    annotateOpName :: Located (MaybeQualified OpName) -> Sem (Reader InspectionContext : r) (Located (Qualified OpName))
    annotateOpName = annotateGenericName NOpName

    qualifyInThisModule :: ToName n => Located (MaybeQualified n) -> Sem (Reader InspectionContext : r) (Located (Qualified n))
    qualifyInThisModule nv@(Located _ (MaybeQualified name' Nothing)) = pure (Qualified name' (thisModule ^. name . unlocated) <$ nv)
    qualifyInThisModule nv@(Located _ (MaybeQualified name' (Just q))) = do
        let qualified = nv $> Qualified name' q
         in if q == thisModule ^. name . unlocated
                then pure qualified
                else throw (QualifiedWithWrongModule (toName <<$>> qualified) (thisModule ^. name . unlocated))

    annotateName :: Located (MaybeQualified Name) -> Sem (Reader InspectionContext : r) (Located (Qualified Name))
    annotateName nv@(Located _ (MaybeQualified (NVarName name') _)) = NVarName <<<$>>> annotateVarName (name' <<$ nv)
    annotateName nv@(Located _ (MaybeQualified (NTypeName name') _)) = NTypeName <<<$>>> annotateTypeName (name' <<$ nv)
    annotateName nv@(Located _ (MaybeQualified (NOpName name') _)) = NOpName <<<$>>> annotateOpName (name' <<$ nv)

    annotateImport :: Import Frontend -> Sem (Reader InspectionContext : r) (Import Annotated)
    annotateImport = traverseOf (_Import . unlocated) (\(Import' name' as' qualified' exposing') -> Import' name' as' qualified' <$> annotateExposing exposing')

    annotateDeclaration :: Declaration Frontend -> Sem (Reader InspectionContext : r) (Declaration Annotated)
    annotateDeclaration =
        traverseOf
            (_Declaration . unlocated)
            ( \(Declaration' module' name' body') -> do
                annotatedName <- qualifyInThisModule name'
                annotatedBody <- annotateDeclarationBody body'
                pure (Declaration' module' annotatedName annotatedBody)
            )

    annotateDeclarationBody :: DeclarationBody Frontend -> Sem (Reader InspectionContext : r) (DeclarationBody Annotated)
    annotateDeclarationBody = traverseOf (_DeclarationBody . unlocated) annotateDeclarationBody'

    annotateDeclarationBody' (Value expr patterns typeAnnotation) = do
        annotatedExpr <- annotateExpr expr
        annotatedPatterns <- traverse annotatePattern patterns
        annotatedTypeAnnotation <- traverse annotateType (typeAnnotation :: Maybe Frontend.TypeAnnotation)

        pure (Value annotatedExpr annotatedPatterns annotatedTypeAnnotation)
    annotateDeclarationBody' (ValueTypeDef type') = do
        annotatedType <- traverseOf unlocated (traverse annotateType) type'
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
        pure (lambda ^. (Annotated._Expr . unlocated))
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
    annotateExpr' (Frontend.Match xs cases) = do
        annotatedXs <- annotateExpr xs
        annotatedCases <- traverse (bitraverse annotatePattern annotateExpr) cases
        pure (Annotated.Match annotatedXs annotatedCases)
    annotateExpr' (Frontend.LetIn lName lPats lExp lBody) = do
        annotatedPats <- traverse annotatePattern lPats
        annotatedExp <- annotateExpr lExp
        annotatedBody <- annotateExpr lBody
        let lambdaExp = unfoldLambda annotatedPats annotatedExp
        pure (Annotated.LetIn lName lambdaExp annotatedBody)
    annotateExpr' (Frontend.Let lName lPats lExp) = do
        annotatedPats <- traverse annotatePattern lPats
        annotatedExp <- annotateExpr lExp
        let lambdaExp = unfoldLambda annotatedPats annotatedExp
        pure (Annotated.Let lName lambdaExp)
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
    annotatePattern' (Frontend.IntegerPattern i) = pure (Annotated.IntegerPattern i)
    annotatePattern' (Frontend.FloatPattern f) = pure (Annotated.FloatPattern f)
    annotatePattern' (Frontend.StringPattern s) = pure (Annotated.StringPattern s)
    annotatePattern' (Frontend.CharPattern c) = pure (Annotated.CharPattern c)

    annotateOp :: Frontend.BinaryOperator -> Sem (Reader InspectionContext : r) Annotated.BinaryOperator
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
    annotateType' (Frontend.RecordType fields) = Annotated.RecordType <$> traverseOf (traverse . _2) annotateType' fields

unfoldLambda :: [Annotated.Pattern] -> Annotated.Expr -> Annotated.Expr
unfoldLambda [] expr = expr
unfoldLambda (p : ps) expr =
    over
        Annotated._Expr
        (\expr' -> Located (expr' ^. sourceRegion) (Annotated.Lambda p (unfoldLambda ps expr)))
        expr