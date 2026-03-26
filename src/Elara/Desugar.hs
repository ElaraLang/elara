{-# LANGUAGE TemplateHaskell #-}

module Elara.Desugar where

import Data.Map qualified as M
import Effectful (Eff, inject)
import Effectful.Error.Static (throwError)
import Effectful.Error.Static qualified as Eff
import Effectful.State.Extra
import Effectful.State.Static.Local qualified as Eff

import Elara.AST.New.Extensions
import Elara.AST.New.Module qualified as NewModule
import Elara.AST.New.Phase (NoExtension (..))
import Elara.AST.New.Phases.Desugared (DesugaredExpressionExtension (..))
import Elara.AST.New.Phases.Desugared qualified as NewD
import Elara.AST.New.Phases.Frontend qualified as Frontend
import Elara.AST.New.Types qualified as New

import Elara.AST.Name hiding (name)
import Elara.AST.Region
import Elara.Data.Pretty (Pretty (..))
import Elara.Desugar.Error
import Elara.Error (runErrorOrReport)
import Elara.Logging
import Elara.Parse.Error (WParseErrorBundle)
import Elara.Query qualified
import Elara.Query.Effects (ConsQueryEffects)
import Rock qualified
import Prelude hiding (Op)

type Desugar a = Eff DesugarPipelineEffects a

type DesugarPipelineEffects = '[Eff.State DesugarState, Eff.Error DesugarError, StructuredDebug]

newtype DesugarState = DesugarState
    { _partialDeclarations :: Map (IgnoreLocation Name) PartialDeclaration
    }
    deriving (Show, Pretty, Semigroup, Monoid)

makeLenses ''DesugarState

getDesugaredModule ::
    ModuleName ->
    Eff
        (ConsQueryEffects '[Eff.Error DesugarError, Rock.Rock Elara.Query.Query])
        (NewModule.Module SourceRegion NewD.Desugared)
getDesugaredModule mn = do
    parsed <- runErrorOrReport @(WParseErrorBundle _ _) $ Rock.fetch $ Elara.Query.ParsedModule mn
    inject $ Eff.evalState (DesugarState mempty) $ desugar parsed

desugar ::
    NewModule.Module SourceRegion Frontend.Frontend ->
    Desugar (NewModule.Module SourceRegion NewD.Desugared)
desugar (NewModule.Module loc (NewModule.Module' name exposing imports decls)) = do
    decls' <- desugarDeclarations name decls
    pure (NewModule.Module loc (NewModule.Module' name (coerceExposing exposing) (map coerceImport imports) decls'))

desugarDeclarations :: Located ModuleName -> [New.Declaration SourceRegion Frontend.Frontend] -> Desugar [New.Declaration SourceRegion NewD.Desugared]
desugarDeclarations mn decls = do
    genPartials decls
    completePartials mn

coerceExposing :: NewModule.Exposing SourceRegion Frontend.Frontend -> NewModule.Exposing SourceRegion NewD.Desugared
coerceExposing NewModule.ExposingAll = NewModule.ExposingAll
coerceExposing (NewModule.ExposingSome exps) = NewModule.ExposingSome (map coerceExposition exps)

coerceExposition :: NewModule.Exposition SourceRegion Frontend.Frontend -> NewModule.Exposition SourceRegion NewD.Desugared
coerceExposition (NewModule.ExposedValue v) = NewModule.ExposedValue v
coerceExposition (NewModule.ExposedOp o) = NewModule.ExposedOp o
coerceExposition (NewModule.ExposedType t) = NewModule.ExposedType t
coerceExposition (NewModule.ExposedTypeAndAllConstructors t) = NewModule.ExposedTypeAndAllConstructors t

coerceImport :: NewModule.Import SourceRegion Frontend.Frontend -> NewModule.Import SourceRegion NewD.Desugared
coerceImport (NewModule.Import loc (NewModule.Import' name as' qual exp')) =
    NewModule.Import loc (NewModule.Import' name as' qual (coerceExposing exp'))

assertPartialNamesEqual :: Eq a => (PartialDeclaration, Located a) -> (PartialDeclaration, Located a) -> Desugar ()
assertPartialNamesEqual (p1, n1) (p2, n2) = if n1 ^. unlocated == n2 ^. unlocated then pass else throwError (PartialNamesNotEqual p1 p2)

mergeAnnotations :: Maybe [New.Annotation SourceRegion NewD.Desugared] -> Maybe [New.Annotation SourceRegion NewD.Desugared] -> Desugar [New.Annotation SourceRegion NewD.Desugared]
mergeAnnotations (Just as) (Just bs) = pure (as <> bs)
mergeAnnotations a b = pure (fromMaybe [] (a <|> b))

mergePartials :: PartialDeclaration -> PartialDeclaration -> Desugar PartialDeclaration
mergePartials p1@(JustDef n sr ty mAnn) p2@(JustLet n' sr' e mAnn') = do
    assertPartialNamesEqual (p1, n) (p2, n')
    ann <- mergeAnnotations mAnn mAnn'
    pure (AllDecl n' (sr <> sr') ty e ann)
mergePartials p1@(JustLet n sr e mAnn) p2@(JustDef n' sr' ty mAnn') = do
    assertPartialNamesEqual (p1, n) (p2, n')
    ann <- mergeAnnotations mAnn mAnn'
    pure (AllDecl n' (sr <> sr') ty e ann)
mergePartials l r = throwError (DuplicateDeclaration l r)

resolvePartialDeclaration :: PartialDeclaration -> Desugar (New.DeclarationBody SourceRegion NewD.Desugared)
resolvePartialDeclaration (Immediate _ body) = pure body
resolvePartialDeclaration (JustDef _ _ ty _) = throwError (DefWithoutLet ty)
resolvePartialDeclaration (JustLet n sr e mAnn) =
    pure (New.DeclarationBody sr (New.ValueDeclaration n e [] Nothing Nothing (fromMaybe [] mAnn)))
resolvePartialDeclaration (AllDecl n sr ty e ann) =
    pure (New.DeclarationBody sr (New.ValueDeclaration n e [] Nothing (Just ty) ann))

genPartials :: [New.Declaration SourceRegion Frontend.Frontend] -> Desugar ()
genPartials = traverse_ genPartial
  where
    genPartial :: New.Declaration SourceRegion Frontend.Frontend -> Desugar ()
    genPartial (New.Declaration wholeDeclRegion (New.Declaration' _mn (New.DeclarationBody bodyLoc body))) =
        genPartial' body
      where
        genPartial' :: New.DeclarationBody' SourceRegion Frontend.Frontend -> Desugar ()
        genPartial' db = do
            partial <- genPartial'' db
            let name = declBodyName db
            let f = insertWithM mergePartials (IgnoreLocation name) partial
            let modifyM g = Eff.get >>= (Eff.put <=< g)
            modifyM (traverseOf partialDeclarations f)

        genPartial'' :: New.DeclarationBody' SourceRegion Frontend.Frontend -> Desugar PartialDeclaration
        genPartial'' (New.ValueDeclaration n e pats _mTy _meta annotations) = do
            e' <- desugarExpr e
            pats' <- traverse desugarPattern pats
            let body' = foldLambda pats' e'
            ann <- traverse desugarAnnotation annotations
            pure (JustLet n wholeDeclRegion body' (Just ann))
        genPartial'' (New.DeclBodyExtension (Frontend.FrontendValueTypeDef n ty annotations)) = do
            ty' <- desugarType ty
            ann <- traverse desugarAnnotation annotations
            pure (JustDef n wholeDeclRegion ty' (Just ann))
        genPartial'' (New.TypeDeclarationBody n vars typeDecl _mKind _meta annotations) = do
            typeDecl' <- desugarTypeDeclaration typeDecl
            ann <- traverse desugarAnnotation annotations
            let decl' = New.TypeDeclarationBody n vars typeDecl' Nothing NoExtension ann
            logDebug $
                "Desugared type declaration at "
                    <> pretty bodyLoc
                    <> " for type "
                    <> pretty (n ^. unlocated)
            pure (Immediate (NTypeName (n ^. unlocated)) (New.DeclarationBody bodyLoc decl'))

        declBodyName :: New.DeclarationBody' SourceRegion Frontend.Frontend -> Located Name
        declBodyName (New.ValueDeclaration n _ _ _ _ _) = NVarName <$> n
        declBodyName (New.TypeDeclarationBody n _ _ _ _ _) = NTypeName <$> n
        declBodyName (New.DeclBodyExtension (Frontend.FrontendValueTypeDef n _ _)) = NVarName <$> n

completePartials :: Located ModuleName -> Desugar [New.Declaration SourceRegion NewD.Desugared]
completePartials mn = do
    partials <- use' partialDeclarations
    decls <-
        M.traverseWithKey
            ( \declName partial -> do
                body <- resolvePartialDeclaration partial
                let locatedName = declName ^. _IgnoreLocation
                let declaration' = New.Declaration' mn body
                let New.DeclarationBody bodyLoc _ = body
                logDebug $
                    "Merging locations:"
                        <> pretty (locatedName ^. sourceRegion)
                        <> " and "
                        <> pretty bodyLoc
                        <> "for declaration "
                        <> pretty (locatedName ^. unlocated)

                let overallLocation = locatedName ^. sourceRegion <> bodyLoc
                pure (New.Declaration overallLocation declaration')
            )
            partials
    pure (M.elems decls)

desugarAnnotation :: New.Annotation SourceRegion Frontend.Frontend -> Desugar (New.Annotation SourceRegion NewD.Desugared)
desugarAnnotation (New.Annotation n args) = New.Annotation n <$> traverse desugarAnnotationArg args

desugarAnnotationArg :: New.AnnotationArg SourceRegion Frontend.Frontend -> Desugar (New.AnnotationArg SourceRegion NewD.Desugared)
desugarAnnotationArg (New.AnnotationArg e) = New.AnnotationArg <$> desugarExpr e

desugarTypeDeclaration :: New.TypeDeclaration SourceRegion Frontend.Frontend -> Desugar (New.TypeDeclaration SourceRegion NewD.Desugared)
desugarTypeDeclaration (New.Alias t) = New.Alias <$> desugarType t
desugarTypeDeclaration (New.ADT constructors) = New.ADT <$> traverse (\(cn, tys) -> (cn,) <$> traverse desugarType tys) constructors

desugarType :: New.Type SourceRegion Frontend.Frontend -> Desugar (New.Type SourceRegion NewD.Desugared)
desugarType (New.Type loc () t') = New.Type loc () <$> desugarType' t'
  where
    desugarType' :: New.Type' SourceRegion Frontend.Frontend -> Desugar (New.Type' SourceRegion NewD.Desugared)
    desugarType' (New.TVar n) = pure (New.TVar n)
    desugarType' (New.TFun t1 t2) = New.TFun <$> desugarType t1 <*> desugarType t2
    desugarType' New.TUnit = pure New.TUnit
    desugarType' (New.TApp t1 t2) = New.TApp <$> desugarType t1 <*> desugarType t2
    desugarType' (New.TUserDefined n) = pure (New.TUserDefined n)
    desugarType' (New.TRecord fields) = New.TRecord <$> traverse (\(n, t) -> (n,) <$> desugarType t) fields
    desugarType' (New.TList t) = New.TList <$> desugarType t
    desugarType' (New.TExtension (TupleType ts)) = New.TExtension . TupleType <$> traverse desugarType ts

desugarExpr :: New.Expr SourceRegion Frontend.Frontend -> Desugar (New.Expr SourceRegion NewD.Desugared)
desugarExpr (New.Expr loc () e') = New.Expr loc () <$> desugarExpr' e'
  where
    desugarExpr' :: New.Expr' SourceRegion Frontend.Frontend -> Desugar (New.Expr' SourceRegion NewD.Desugared)
    desugarExpr' (New.EInt i) = pure (New.EInt i)
    desugarExpr' (New.EFloat f) = pure (New.EFloat f)
    desugarExpr' (New.EString s) = pure (New.EString s)
    desugarExpr' (New.EChar c) = pure (New.EChar c)
    desugarExpr' New.EUnit = pure New.EUnit
    desugarExpr' (New.EVar NoExtension v) = pure (New.EVar NoExtension v)
    desugarExpr' (New.ECon NoExtension c) = pure (New.ECon NoExtension c)
    desugarExpr' (New.ELam NoExtension pat body) = do
        pat' <- desugarPattern pat
        body' <- desugarExpr body
        pure (New.ELam NoExtension pat' body')
    desugarExpr' (New.EApp NoExtension e1 e2) = New.EApp NoExtension <$> desugarExpr e1 <*> desugarExpr e2
    desugarExpr' (New.ETyApp e1 ty) = New.ETyApp <$> desugarExpr e1 <*> desugarType ty
    desugarExpr' (New.EIf a b c) = New.EIf <$> desugarExpr a <*> desugarExpr b <*> desugarExpr c
    desugarExpr' (New.EMatch e cases) = do
        e'' <- desugarExpr e
        cases' <- traverse (bitraverse desugarPattern desugarExpr) cases
        pure (New.EMatch e'' cases')
    desugarExpr' (New.ELetIn NoExtension n e body) =
        New.ELetIn NoExtension n <$> desugarExpr e <*> desugarExpr body
    desugarExpr' (New.ELet NoExtension n e) =
        New.ELet NoExtension n <$> desugarExpr e
    desugarExpr' (New.EBlock es) = New.EBlock <$> traverse desugarExpr es
    desugarExpr' (New.EAnn e ty) = New.EAnn <$> desugarExpr e <*> desugarType ty
    desugarExpr' (New.EExtension ext) = desugarExprExtension ext

desugarExprExtension :: Frontend.FrontendExpressionExtension SourceRegion -> Desugar (New.Expr' SourceRegion NewD.Desugared)
desugarExprExtension (Frontend.FrontendMultiLam pats body) = do
    pats' <- traverse desugarPattern pats
    body' <- desugarExpr body
    let folded = foldLambda pats' body'
    let New.Expr _ () inner = folded
    pure inner
desugarExprExtension (Frontend.FrontendLetWithPatterns n pats e) = do
    pats' <- traverse desugarPattern pats
    e' <- desugarExpr e
    pure (New.ELet NoExtension n (foldLambda pats' e'))
desugarExprExtension (Frontend.FrontendLetInWithPatterns n pats e body) = do
    pats' <- traverse desugarPattern pats
    e' <- desugarExpr e
    body' <- desugarExpr body
    pure (New.ELetIn NoExtension n (foldLambda pats' e') body')
desugarExprExtension (Frontend.FrontendBinaryOperator ext) = do
    ext' <- desugarBinaryOperatorExtension ext
    pure (New.EExtension (DesugaredBinaryOperator ext'))
desugarExprExtension (Frontend.FrontendInParens (InParensExpression e)) = do
    e' <- desugarExpr e
    pure (New.EExtension (DesugaredInParens (InParensExpression e')))
desugarExprExtension (Frontend.FrontendList (ListExpression es)) = do
    es' <- traverse desugarExpr es
    pure (New.EExtension (DesugaredList (ListExpression es')))
desugarExprExtension (Frontend.FrontendTuple (TupleExpression es)) = do
    es' <- traverse desugarExpr es
    pure (New.EExtension (DesugaredTuple (TupleExpression es')))

desugarBinaryOperatorExtension :: BinaryOperatorExtension SourceRegion Frontend.Frontend -> Desugar (BinaryOperatorExtension SourceRegion NewD.Desugared)
desugarBinaryOperatorExtension (BinaryOperatorExpression op a b) = do
    op' <- desugarBinaryOperator op
    a' <- desugarExpr a
    b' <- desugarExpr b
    pure (BinaryOperatorExpression op' a' b')

desugarBinaryOperator :: New.BinaryOperator SourceRegion Frontend.Frontend -> Desugar (New.BinaryOperator SourceRegion NewD.Desugared)
desugarBinaryOperator (New.SymOp loc occ) = pure (New.SymOp loc occ)
desugarBinaryOperator (New.InfixedOp loc occ) = pure (New.InfixedOp loc occ)

desugarPattern :: New.Pattern SourceRegion Frontend.Frontend -> Desugar (New.Pattern SourceRegion NewD.Desugared)
desugarPattern p@(New.Pattern loc meta p') = do
    meta' <- traverse desugarType meta
    p'' <- desugarPattern' p'
    pure (New.Pattern loc meta' p'')
  where
    desugarPattern' :: New.Pattern' SourceRegion Frontend.Frontend -> Desugar (New.Pattern' SourceRegion NewD.Desugared)
    desugarPattern' (New.PVar v) = pure (New.PVar v)
    desugarPattern' (New.PCon c pats) = New.PCon c <$> traverse desugarPattern pats
    desugarPattern' New.PWildcard = pure New.PWildcard
    desugarPattern' (New.PInt i) = pure (New.PInt i)
    desugarPattern' (New.PFloat f) = pure (New.PFloat f)
    desugarPattern' (New.PString s) = pure (New.PString s)
    desugarPattern' (New.PChar c) = pure (New.PChar c)
    desugarPattern' New.PUnit = pure New.PUnit
    desugarPattern' (New.PExtension ext) = desugarPatternExtension ext

    desugarPatternExtension :: ListTuplePatternExtension SourceRegion Frontend.Frontend -> Desugar (New.Pattern' SourceRegion NewD.Desugared)
    desugarPatternExtension (ListPattern pats) = New.PExtension . ListPattern <$> traverse desugarPattern pats
    desugarPatternExtension (ConsPattern l r) = (\l' r' -> New.PExtension (ConsPattern l' r')) <$> desugarPattern l <*> desugarPattern r
    desugarPatternExtension (TuplePattern (_ :| [])) = throwError (TuplePatternTooShort p)
    desugarPatternExtension (TuplePattern pats) = do
        pats' <- traverse desugarPattern pats
        pure (New.PExtension (TuplePattern pats'))

{- | Lambdas need quite a lot of desugaring - they need to be unfolded into a series of nested lambdas, and then each pattern needs to be converted into a match expression.

For example, @\a (b, c) 1 -> e@ becomes
@\a -> \b_1 -> \c_1 -> match b_1 with
                            (b, c) -> match 1 with
                                        1 -> e@

However, converting the matches would require renaming, and we're not able to do that yet.
Instead, we unfold the lambda, but keep the patterns, and the renamer handles the match conversion.
-}
foldLambda :: [New.Pattern SourceRegion NewD.Desugared] -> New.Expr SourceRegion NewD.Desugared -> New.Expr SourceRegion NewD.Desugared
foldLambda [] e = e
foldLambda (p@(New.Pattern pLoc _ _) : ps) e =
    let inner = foldLambda ps e
        New.Expr innerLoc _ _ = inner
     in New.Expr (pLoc <> innerLoc) () (New.ELam NoExtension p inner)
