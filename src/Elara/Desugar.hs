{-# LANGUAGE TemplateHaskell #-}

module Elara.Desugar where

import Data.Map qualified as M
import Effectful (Eff, inject)
import Effectful.Error.Static (throwError)
import Effectful.Error.Static qualified as Eff
import Effectful.State.Extra
import Effectful.State.Static.Local qualified as Eff
import Elara.AST.Extensions
import Elara.AST.Location
import Elara.AST.Module qualified as NewModule
import Elara.AST.Name hiding (name)
import Elara.AST.Phase (NoExtension (..))
import Elara.AST.PhaseCoerce (PhaseCoerce (..))
import Elara.AST.Phases.Desugared
import Elara.AST.Phases.Frontend qualified as Frontend
import Elara.AST.Region
import Elara.AST.Types qualified as New
import Elara.Data.Pretty (Pretty (..))
import Elara.Desugar.Common
import Elara.Desugar.Error
import Elara.Error (runErrorAsElaraError)
import Elara.Logging
import Elara.Parse.Error (WParseErrorBundle)
import Elara.Query qualified
import Elara.Query.Effects (ConsQueryEffects)
import Rock qualified
import Prelude hiding (Op)

instance PhaseCoerce (NewModule.Exposing loc Frontend.Frontend) (NewModule.Exposing loc Desugared)

instance PhaseCoerce (NewModule.Exposition loc Frontend.Frontend) (NewModule.Exposition loc Desugared)

instance PhaseCoerce (NewModule.Import loc Frontend.Frontend) (NewModule.Import loc Desugared)

instance PhaseCoerce (NewModule.Import' loc Frontend.Frontend) (NewModule.Import' loc Desugared)

instance PhaseCoerce (NewModule.ImportExposingOrHiding loc Frontend.Frontend) (NewModule.ImportExposingOrHiding loc Desugared)

type Desugar a = Eff DesugarPipelineEffects a

type DesugarPipelineEffects = '[Eff.State DesugarState, Eff.Error DesugarError, StructuredDebug]

newtype DesugarState = DesugarState
    { _partialDeclarations :: Map Name PartialDeclaration
    }
    deriving (Show, Pretty, Semigroup, Monoid)

makeLenses ''DesugarState

getDesugaredModule ::
    ModuleName ->
    Eff
        (ConsQueryEffects '[Eff.Error DesugarError, Rock.Rock Elara.Query.Query])
        (NewModule.Module SourceRegion Desugared)
getDesugaredModule mn = do
    parsed <- runErrorAsElaraError @(WParseErrorBundle _ _) $ Rock.fetch $ Elara.Query.ParsedModule mn
    inject $ Eff.evalState (DesugarState mempty) $ desugar parsed

desugar ::
    NewModule.Module SourceRegion Frontend.Frontend ->
    Desugar (NewModule.Module SourceRegion Desugared)
desugar (NewModule.Module loc (NewModule.Module' name exposing imports decls)) = do
    decls' <- desugarDeclarations name decls
    pure (NewModule.Module loc (NewModule.Module' name (phaseCoerce exposing) (map phaseCoerce imports) decls'))

desugarDeclarations ::
    TaggedLocate ModuleNode SourceRegion ModuleName ->
    [New.Declaration SourceRegion Frontend.Frontend] ->
    Desugar [New.Declaration SourceRegion Desugared]
desugarDeclarations mn decls = do
    genPartials decls
    completePartials mn

assertPartialNamesEqual :: (PartialDeclaration, TaggedLocate VarNode SourceRegion VarName) -> (PartialDeclaration, TaggedLocate VarNode SourceRegion VarName) -> Desugar ()
assertPartialNamesEqual (p1, n1) (p2, n2) = if n1 ^. unlocated == n2 ^. unlocated then pass else throwError (PartialNamesNotEqual p1 p2)

mergeAnnotations :: Maybe [New.Annotation SourceRegion Desugared] -> Maybe [New.Annotation SourceRegion Desugared] -> Desugar [New.Annotation SourceRegion Desugared]
mergeAnnotations (Just as) (Just bs) = pure (as <> bs)
mergeAnnotations a b = pure (fromMaybe [] (a <|> b))

mergePartials :: PartialDeclaration -> PartialDeclaration -> Desugar PartialDeclaration
mergePartials p1@(JustDef n sr ty mAnn) p2@(JustLet n' sr' e mAnn') = do
    assertPartialNamesEqual (p1, n) (p2, n')
    ann <- mergeAnnotations mAnn mAnn'
    pure (AllDecl n' (sr <.> sr') ty e ann)
mergePartials p1@(JustLet n sr e mAnn) p2@(JustDef n' sr' ty mAnn') = do
    assertPartialNamesEqual (p1, n) (p2, n')
    ann <- mergeAnnotations mAnn mAnn'
    pure (AllDecl n' (sr <.> sr') ty e ann)
mergePartials l r = throwError (DuplicateDeclaration l r)

resolvePartialDeclaration :: PartialDeclaration -> Desugar (New.DeclarationBody SourceRegion Desugared)
resolvePartialDeclaration (JustDef _ _ ty _) = throwError (DefWithoutLet ty)
resolvePartialDeclaration (JustLet n sr e mAnn) =
    pure (New.DeclarationBody sr (New.ValueDeclaration n e () () Nothing (fromMaybe [] mAnn)))
resolvePartialDeclaration (AllDecl n sr ty e ann) =
    pure (New.DeclarationBody sr (New.ValueDeclaration n e () () (Just ty) ann))
resolvePartialDeclaration (Immediate (New.Declaration _ (New.Declaration' _ body))) =
    pure body

genPartials :: [New.Declaration SourceRegion Frontend.Frontend] -> Desugar ()
genPartials = traverse_ genPartial
  where
    genPartial :: New.Declaration SourceRegion Frontend.Frontend -> Desugar ()
    genPartial (New.Declaration wholeDeclRegion (New.Declaration' mn (New.DeclarationBody bodyLoc body))) =
        genPartial' body
      where
        genPartial' :: New.DeclarationBody' SourceRegion Frontend.Frontend -> Desugar ()
        genPartial' db = do
            partial <- genPartial'' db
            let name = declBodyName db
            let f = insertWithM mergePartials (name ^. unlocated) partial
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

            let overallLocation = getLocation n <.> bodyLoc
            let finalBody = New.DeclarationBody bodyLoc decl'
            let finalDecl = New.Declaration overallLocation (New.Declaration' mn finalBody)
            pure (Immediate finalDecl)

        declBodyName :: New.DeclarationBody' SourceRegion Frontend.Frontend -> TaggedLocate VarNode SourceRegion Name
        declBodyName (New.ValueDeclaration n _ _ _ _ _) = fmap toName n
        declBodyName (New.TypeDeclarationBody n _ _ _ _ _) = NameType <$> retag @VarNode n
        declBodyName (New.DeclBodyExtension (Frontend.FrontendValueTypeDef n _ _)) = fmap toName n

completePartials :: TaggedLocate ModuleNode SourceRegion ModuleName -> Desugar [New.Declaration SourceRegion Desugared]
completePartials mn = do
    partials <- use' partialDeclarations
    traverse (completePartial mn) (M.elems partials)

completePartial ::
    TaggedLocate ModuleNode SourceRegion ModuleName ->
    PartialDeclaration ->
    Desugar (New.Declaration SourceRegion Desugared)
completePartial _mn (Immediate decl) = pure decl
completePartial mn partial = do
    body <- resolvePartialDeclaration partial

    let locatedName = getPartialName partial
    let declaration' = New.Declaration' mn body
    let New.DeclarationBody bodyLoc _ = body

    logDebug $
        "Merging locations for declaration " <> pretty (locatedName ^. unlocated)
    let overallLocation = getLocation locatedName <.> bodyLoc

    pure (New.Declaration overallLocation declaration')

desugarAnnotation :: New.Annotation SourceRegion Frontend.Frontend -> Desugar (New.Annotation SourceRegion Desugared)
desugarAnnotation (New.Annotation n args) = New.Annotation n <$> traverse desugarAnnotationArg args

desugarAnnotationArg :: New.AnnotationArg SourceRegion Frontend.Frontend -> Desugar (New.AnnotationArg SourceRegion Desugared)
desugarAnnotationArg (New.AnnotationArg e) = New.AnnotationArg <$> desugarExpr e

desugarTypeDeclaration :: New.TypeDeclaration SourceRegion Frontend.Frontend -> Desugar (New.TypeDeclaration SourceRegion Desugared)
desugarTypeDeclaration (New.Alias t) = New.Alias <$> desugarType t
desugarTypeDeclaration (New.ADT constructors) = New.ADT <$> traverse (\(cn, tys) -> (cn,) <$> traverse desugarType tys) constructors

desugarType :: New.Type SourceRegion Frontend.Frontend -> Desugar (New.Type SourceRegion Desugared)
desugarType (New.Type loc () t') = New.Type loc () <$> desugarType' t'
  where
    desugarType' :: New.Type' SourceRegion Frontend.Frontend -> Desugar (New.Type' SourceRegion Desugared)
    desugarType' (New.TVar n) = pure (New.TVar n)
    desugarType' (New.TFun t1 t2) = New.TFun <$> desugarType t1 <*> desugarType t2
    desugarType' New.TUnit = pure New.TUnit
    desugarType' (New.TApp t1 t2) = New.TApp <$> desugarType t1 <*> desugarType t2
    desugarType' (New.TUserDefined n) = pure (New.TUserDefined n)
    desugarType' (New.TRecord fields) = New.TRecord <$> traverse (\(n, t) -> (n,) <$> desugarType t) fields
    desugarType' (New.TList t) = New.TList <$> desugarType t
    desugarType' (New.TExtension (TupleType ts)) = New.TExtension . TupleType <$> traverse desugarType ts

desugarExpr :: New.Expr SourceRegion Frontend.Frontend -> Desugar (New.Expr SourceRegion Desugared)
desugarExpr (New.Expr loc () e') = New.Expr loc () <$> desugarExpr' e'
  where
    desugarExpr' :: New.Expr' SourceRegion Frontend.Frontend -> Desugar (New.Expr' SourceRegion Desugared)
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

desugarExprExtension :: Frontend.FrontendExpressionExtension SourceRegion -> Desugar (New.Expr' SourceRegion Desugared)
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

desugarBinaryOperatorExtension :: BinaryOperatorExtension SourceRegion Frontend.Frontend -> Desugar (BinaryOperatorExtension SourceRegion Desugared)
desugarBinaryOperatorExtension (BinaryOperatorExpression op a b) = do
    op' <- desugarBinaryOperator op
    a' <- desugarExpr a
    b' <- desugarExpr b
    pure (BinaryOperatorExpression op' a' b')

desugarBinaryOperator :: New.BinaryOperator SourceRegion Frontend.Frontend -> Desugar (New.BinaryOperator SourceRegion Desugared)
desugarBinaryOperator (New.SymOp loc occ) = pure (New.SymOp loc occ)
desugarBinaryOperator (New.InfixedOp loc occ) = pure (New.InfixedOp loc occ)

desugarPattern :: New.Pattern SourceRegion Frontend.Frontend -> Desugar (New.Pattern SourceRegion Desugared)
desugarPattern p@(New.Pattern loc meta p') = do
    meta' <- traverse desugarType meta
    p'' <- desugarPattern' p'
    pure (New.Pattern loc meta' p'')
  where
    desugarPattern' :: New.Pattern' SourceRegion Frontend.Frontend -> Desugar (New.Pattern' SourceRegion Desugared)
    desugarPattern' (New.PVar v) = pure (New.PVar v)
    desugarPattern' (New.PCon c pats) = New.PCon c <$> traverse desugarPattern pats
    desugarPattern' New.PWildcard = pure New.PWildcard
    desugarPattern' (New.PInt i) = pure (New.PInt i)
    desugarPattern' (New.PFloat f) = pure (New.PFloat f)
    desugarPattern' (New.PString s) = pure (New.PString s)
    desugarPattern' (New.PChar c) = pure (New.PChar c)
    desugarPattern' New.PUnit = pure New.PUnit
    desugarPattern' (New.PExtension ext) = desugarPatternExtension p ext

    desugarPatternExtension :: New.Pattern SourceRegion Frontend.Frontend -> ListTuplePatternExtension SourceRegion Frontend.Frontend -> Desugar (New.Pattern' SourceRegion Desugared)
    desugarPatternExtension _ (ListPattern pats) = New.PExtension . ListPattern <$> traverse desugarPattern pats
    desugarPatternExtension _ (ConsPattern l r) = (\l' r' -> New.PExtension (ConsPattern l' r')) <$> desugarPattern l <*> desugarPattern r
    desugarPatternExtension p (TuplePattern (_ :| [])) = throwError (TuplePatternTooShort p)
    desugarPatternExtension _ (TuplePattern pats) = do
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
foldLambda :: [New.Pattern SourceRegion Desugared] -> New.Expr SourceRegion Desugared -> New.Expr SourceRegion Desugared
foldLambda [] e = e
foldLambda (p@(New.Pattern pLoc _ _) : ps) e =
    let inner@(New.Expr innerLoc _ _) = foldLambda ps e
        newLoc = pLoc <.> innerLoc
     in New.Expr newLoc () (New.ELam NoExtension p inner)
