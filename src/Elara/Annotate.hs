{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Elara.Annotate where

import Control.Lens (over, to, traverseOf, view, (^.), (^..), _2)
import Data.List.NonEmpty ((<|))
import Data.Map qualified as M
import Elara.AST.Annotated qualified as Annotated
import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Module (Exposing (..), Exposition (..), HasDeclarations (declarations), HasExposing (exposing), HasImports (imports), Import (..), Import' (Import'), Module (..), Module' (..), _Import, _Module)
import Elara.AST.Module.Inspection (CleanedInspectionContext, ContextBuildingError, ContextCleaningError, buildContext, search, verifyContext)
import Elara.AST.Name (MaybeQualified (MaybeQualified), ModuleName, Name (..), NameLike (fullNameText), OpName, Qualified (Qualified), ToName (toName), TypeName, VarName)
import Elara.AST.Region (IgnoreLocation (IgnoreLocation), Located (Located), sourceRegion, sourceRegionToDiagnosePosition, unlocated, _IgnoreLocation)
import Elara.AST.Select (Annotated, Frontend, HasName (name))
import Elara.Error (ReportableError (report))
import Elara.Error.Codes qualified as Codes
import Elara.Error.Effect
import Error.Diagnose hiding (addReport)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, runError, throw)
import Polysemy.Reader (Reader, ask, runReader)
import Polysemy.State
import Prelude hiding (Reader, Type, ask, evalState, execState, modify, runReader, runState)

type Modules = M.Map ModuleName (Module Frontend)

data AnnotationError
    = -- | The name is not defined in any known module
      UnknownName (Located (MaybeQualified Name))
    | DefWithoutLet (Located (MaybeQualified Name))
    | TooManyDeclarations (NonEmpty (Located Frontend.Declaration'))
    | ContextBuildingError (ContextBuildingError Frontend)
    | ContextCleaningError (NonEmpty (ContextCleaningError Frontend))
    | QualifiedWithWrongModule (Located (Qualified Name)) ModuleName
    | DuplicateDefinition (Located (Qualified Name)) (NonEmpty (Located (Qualified Name)))
    deriving (Show)

instance ReportableError AnnotationError where
    report (UnknownName ln) = do
        let pos = sourceRegionToDiagnosePosition $ ln ^. sourceRegion
        writeReport $
            Err
                (Just Codes.unknownName)
                ("Unknown name: " <> fullNameText (ln ^. unlocated))
                [(pos, This "referenced here")]
                []
    report (DefWithoutLet (Located sr n)) = do
        let pos = sourceRegionToDiagnosePosition sr
        writeReport $
            Err
                (Just Codes.defWithoutLet)
                ("Definition without let: " <> fullNameText n)
                [(pos, This "")]
                [ Hint "Try adding a let after this definition"
                ]
    report (TooManyDeclarations decls) = do
        let poses = sourceRegionToDiagnosePosition . view sourceRegion <$> decls
        writeReport $
            Err
                (Just Codes.tooManyDeclarations)
                "Too many declarations"
                (toList $ (,Where "here") <$> poses)
                [ Hint "Try moving some of these declarations into a separate module"
                ]
    report (ContextBuildingError e) = report e
    report (ContextCleaningError e) = traverse_ report e
    report (QualifiedWithWrongModule (Located sr (Qualified n qual)) modName) = do
        let pos = sourceRegionToDiagnosePosition sr
        writeReport $
            Err
                (Just Codes.qualifiedWithWrongModule)
                ("Qualification of name doesn't match module name: " <> fullNameText modName)
                [(pos, This "")]
                [ Note "If you choose to qualify your names, the qualification has to match the name of your module - you can't just pick any name."
                , Hint $ "Try changing " <> fullNameText qual <> " to " <> fullNameText modName
                , Hint $ "Try changing " <> fullNameText qual <> "." <> fullNameText n <> " to " <> fullNameText n
                , Hint $ "Try renaming your module to " <> fullNameText qual
                ]
    report (DuplicateDefinition (Located sr1 n1) others) = do
        let pos1 = sourceRegionToDiagnosePosition sr1
        let otherPoses = view (sourceRegion . to sourceRegionToDiagnosePosition) <$> others
        let markers = (pos1, This "defined here") <| ((,Where "also defined here") <$> otherPoses)
        writeReport $
            Err
                (Just Codes.duplicateDefinition)
                ("Duplicate definition of " <> fullNameText n1)
                (toList markers)
                [Note "If you're trying to do pattern matching a-la Haskell, you can't do that in Elara. Make a single definition and use a match expression"]

searchOrThrow :: (Member (Error AnnotationError) r, Member (Reader (CleanedInspectionContext Frontend)) r) => Located (MaybeQualified Name) -> Sem r ModuleName
searchOrThrow l@(Located _ mq) = do
    searched <- search mq
    case searched of
        Nothing -> throw (UnknownName l)
        Just a -> pure a

wrapCtxError :: Member (Error AnnotationError) r => Sem (Error (ContextBuildingError Frontend) : r) b -> Sem r b
wrapCtxError sem = do
    runError sem >>= \case
        Left e -> throw (ContextBuildingError e)
        Right a -> pure a

wrapCtxCleanError :: Member (Error AnnotationError) r => Sem (Error (NonEmpty (ContextCleaningError Frontend)) : r) b -> Sem r b
wrapCtxCleanError sem = do
    runError sem >>= \case
        Left e -> throw (ContextCleaningError e)
        Right a -> pure a

-- verifyUniqueDeclarations ::
--     forall r.
--     (Member (Error AnnotationError) r) =>
--     Module Frontend ->
--     Sem r ()
-- verifyUniqueDeclarations thisModule = do
--     let declNames = thisModule ^.. declarations . traverse . Frontend._Declaration . unlocated . name :: [Located (MaybeQualified Name)]
--     qualifiedNames <- traverse (qualifyInModule thisModule) declNames
--     let grouped = M.fromListWith (<>) (qualifiedNames <&> \n -> (n ^. unlocated, pure n)) :: Map (Qualified Name) (NonEmpty (Located (Qualified Name)))
--     for_ (M.toList grouped) $ \(_', locs) ->
--         case locs of
--             _ :| [] -> pass
--             a :| (b : xs) -> throw (DuplicateDefinition a (b :| xs))

qualifyInModule ::
    (ToName n, Member (Error AnnotationError) r) =>
    Module Frontend ->
    Located (MaybeQualified n) ->
    Sem r (Located (Qualified n))
qualifyInModule thisModule nv@(Located _ (MaybeQualified name' Nothing)) = pure (Qualified name' (thisModule ^. name . unlocated) <$ nv)
qualifyInModule thisModule nv@(Located _ (MaybeQualified name' (Just q))) = do
    let qualified = nv $> Qualified name' q
     in if q == thisModule ^. name . unlocated
            then pure qualified
            else throw (QualifiedWithWrongModule (toName <<$>> qualified) (thisModule ^. name . unlocated))

annotateAndMergeDeclarations ::
    forall r.
    ( Member (Reader (CleanedInspectionContext Frontend)) r
    , Member (Error AnnotationError) r
    ) =>
    [Located Frontend.Declaration'] ->
    Sem r [Located Annotated.Declaration']
annotateAndMergeDeclarations decls = evalState M.empty $ do
    let grouped = M.fromListWith (<>) (decls <&> \n -> (n ^. unlocated . name . to IgnoreLocation, pure @NonEmpty n))
    for_ (M.toList grouped) $ \(declName, namedDecls) -> do
        let letAndMaybeDef e pats type' = do
                annotatedExpr <- annotateExpr e
                annotatedPatterns <- traverse annotatePattern pats
                type'' <- traverse (traverseOf unlocated annotateTypeAnnotation) type'
                let unfolded = unfoldLambda annotatedPatterns annotatedExpr
                let annotated = Annotated.Value type'' unfolded
                modify (M.insert declName annotated)
        case (^. unlocated . Frontend.declaration'Body . Frontend._DeclarationBody . unlocated) <$> namedDecls of
            Frontend.Value e pats :| [] -> do
                letAndMaybeDef e pats Nothing
            Frontend.ValueTypeDef _ :| [] -> do
                throw (DefWithoutLet (declName ^. _IgnoreLocation))
            Frontend.Value e pats :| [Frontend.ValueTypeDef type'] -> do
                letAndMaybeDef e pats (Just type')
            Frontend.ValueTypeDef type' :| [Frontend.Value e pats] -> do
                letAndMaybeDef e pats (Just type')
            _ -> do
                throw (TooManyDeclarations namedDecls)
    pure []

annotateModule ::
    forall r.
    (Member (Reader Modules) r, Member (Error AnnotationError) r) =>
    Module Frontend ->
    Sem r (Module Annotated)
annotateModule thisModule = traverseOf (_Module . unlocated) (const annotate) thisModule
  where
    annotate :: Sem r (Module' Annotated)
    annotate = do
        -- verifyUniqueDeclarations thisModule
        modules <- ask
        context <- wrapCtxError $ buildContext thisModule modules
        cleanedContext <- wrapCtxCleanError $ runReader context (verifyContext @Frontend)
        exposing' <- runReader cleanedContext $ annotateExposing (thisModule ^. exposing)
        imports' <- runReader cleanedContext (traverse annotateImport (thisModule ^. imports))

        declarations' <- runReader cleanedContext (annotateAndMergeDeclarations (thisModule ^.. declarations . traverse . Frontend._Declaration))
        pure (Module' (thisModule ^. name) exposing' imports' (Annotated.Declaration <$> declarations'))

    annotateExposing :: Exposing Frontend -> Sem (Reader (CleanedInspectionContext Frontend) : r) (Exposing Annotated)
    annotateExposing ExposingAll = pure ExposingAll
    annotateExposing (ExposingSome expos) = ExposingSome <$> traverse annotateExposition expos

    annotateExposition :: Exposition Frontend -> Sem (Reader (CleanedInspectionContext Frontend) : r) (Exposition Annotated)
    annotateExposition (ExposedValue name') = ExposedValue <$> annotateVarName name'
    annotateExposition (ExposedOp name') = ExposedOp <$> annotateOpName name'
    annotateExposition (ExposedType name') = ExposedType <$> annotateTypeName name'
    annotateExposition (ExposedTypeAndAllConstructors name') = ExposedTypeAndAllConstructors <$> annotateTypeName name'

    annotateImport :: Import Frontend -> Sem (Reader (CleanedInspectionContext Frontend) : r) (Import Annotated)
    annotateImport = traverseOf (_Import . unlocated) (\(Import' name' as' qualified' exposing') -> Import' name' as' qualified' <$> annotateExposing exposing')

unfoldLambda :: [Annotated.Pattern] -> Annotated.Expr -> Annotated.Expr
unfoldLambda [] expr = expr
unfoldLambda (p : ps) expr =
    over
        Annotated._Expr
        (\expr' -> Located (expr' ^. sourceRegion) (Annotated.Lambda p (unfoldLambda ps expr)))
        expr

annotateExpr ::
    ( Member (Reader (CleanedInspectionContext Frontend)) r
    , Member (Error AnnotationError) r
    ) =>
    Frontend.Expr ->
    Sem r Annotated.Expr
annotateExpr (Frontend.Expr expr') = Annotated.Expr <$> traverse annotateExpr' expr'

annotateExpr' ::
    ( Member (Error AnnotationError) r
    , Member (Reader (CleanedInspectionContext Frontend)) r
    ) =>
    Frontend.Expr' ->
    Sem r Annotated.Expr'
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

annotatePattern ::
    ( Member (Reader (CleanedInspectionContext Frontend)) r
    , Member (Error AnnotationError) r
    ) =>
    Frontend.Pattern ->
    Sem r Annotated.Pattern
annotatePattern (Frontend.Pattern pattern') = Annotated.Pattern <$> traverse annotatePattern' pattern'
annotatePattern' ::
    ( Member (Error AnnotationError) r
    , Member (Reader (CleanedInspectionContext Frontend)) r
    ) =>
    Frontend.Pattern' ->
    Sem r Annotated.Pattern'
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

annotateOp ::
    ( Member (Reader (CleanedInspectionContext Frontend)) r
    , Member (Error AnnotationError) r
    ) =>
    Frontend.BinaryOperator ->
    Sem r Annotated.BinaryOperator
annotateOp (Frontend.MkBinaryOperator o) = Annotated.MkBinaryOperator <$> traverse annotateOp' o
  where
    annotateOp' ::
        ( Member (Error AnnotationError) r
        , Member (Reader (CleanedInspectionContext Frontend)) r
        ) =>
        Frontend.BinaryOperator' ->
        Sem r Annotated.BinaryOperator'
    annotateOp' (Frontend.Op o') = Annotated.Op <$> annotateOpName o'
    annotateOp' (Frontend.Infixed o') = Annotated.Infixed <$> annotateVarName o'

annotateTypeAnnotation :: (Member (Reader (CleanedInspectionContext Frontend)) r, Member (Error AnnotationError) r) => Frontend.TypeAnnotation -> Sem r Annotated.TypeAnnotation
annotateTypeAnnotation (Frontend.TypeAnnotation annotationName type') = Annotated.TypeAnnotation <$> annotateName annotationName <*> annotateType type'

annotateType :: (Member (Reader (CleanedInspectionContext Frontend)) r, Member (Error AnnotationError) r) => Frontend.Type -> Sem r Annotated.Type
annotateType (Frontend.TypeVar tv) = pure (Annotated.TypeVar tv)
annotateType (Frontend.FunctionType t1 t2) = Annotated.FunctionType <$> annotateType t1 <*> annotateType t2
annotateType Frontend.UnitType = pure Annotated.UnitType
annotateType (Frontend.TypeConstructorApplication ctor arg) = Annotated.TypeConstructorApplication <$> annotateType ctor <*> annotateType arg
annotateType (Frontend.UserDefinedType tn) = Annotated.UserDefinedType <$> annotateTypeName tn
annotateType (Frontend.RecordType fields) = Annotated.RecordType <$> traverseOf (traverse . _2) annotateType fields

annotateVarName ::
    (Member (Reader (CleanedInspectionContext Frontend)) r, Member (Error AnnotationError) r) =>
    Located (MaybeQualified VarName) ->
    Sem r (Located (Qualified VarName))
annotateVarName = annotateGenericName NVarName

annotateTypeName ::
    (Member (Reader (CleanedInspectionContext Frontend)) r, Member (Error AnnotationError) r) =>
    Located (MaybeQualified TypeName) ->
    Sem r (Located (Qualified TypeName))
annotateTypeName = annotateGenericName NTypeName

annotateOpName ::
    (Member (Reader (CleanedInspectionContext Frontend)) r, Member (Error AnnotationError) r) =>
    Located (MaybeQualified OpName) ->
    Sem r (Located (Qualified OpName))
annotateOpName = annotateGenericName NOpName

annotateGenericName ::
    ( Member (Error AnnotationError) r
    , Member (Reader (CleanedInspectionContext Frontend)) r
    ) =>
    (a -> Name) ->
    Located (MaybeQualified a) ->
    Sem r (Located (Qualified a))
annotateGenericName ctor nv@(Located _ (MaybeQualified name' mMod)) = case mMod of
    Just qual -> pure (nv $> Qualified name' qual)
    Nothing -> do
        q <- searchOrThrow (ctor <<$>> nv)
        pure (nv $> Qualified name' q)
annotateName ::
    ( Member (Reader (CleanedInspectionContext Frontend)) r
    , Member (Error AnnotationError) r
    ) =>
    Located (MaybeQualified Name) ->
    Sem r (Located (Qualified Name))
annotateName nv@(Located _ (MaybeQualified (NVarName name') _)) = NVarName <<<$>>> annotateVarName (name' <<$ nv)
annotateName nv@(Located _ (MaybeQualified (NTypeName name') _)) = NTypeName <<<$>>> annotateTypeName (name' <<$ nv)
annotateName nv@(Located _ (MaybeQualified (NOpName name') _)) = NOpName <<<$>>> annotateOpName (name' <<$ nv)
