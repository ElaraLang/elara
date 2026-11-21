{-# LANGUAGE TemplateHaskell #-}

module Elara.Desugar where

import Effectful (Eff, inject, (:>))
import Effectful.Error.Static (throwError)

import Data.Map qualified as M
import Effectful.Error.Static qualified as Eff
import Effectful.State.Static.Local qualified as Eff

import Effectful.State.Extra
import Elara.AST.Extensions
import Elara.AST.Location
import Elara.AST.Module
import Elara.AST.Name hiding (name)
import Elara.AST.Phase (NoExtension (..))
import Elara.AST.PhaseCoerce (PhaseCoerce (..))
import Elara.AST.Phases.Desugared
import Elara.AST.Phases.Frontend as Frontend
import Elara.AST.Region
import Elara.AST.Types
import Elara.Data.Pretty (Pretty (..))
import Elara.Desugar.Common
import Elara.Desugar.Error
import Elara.Error (runErrorAsElaraError)
import Elara.Logging
import Elara.Query.Effects (QueryEffects)
import Prelude hiding (Op)

import Elara.Query qualified
import Rock qualified

instance PhaseCoerce (Exposing loc Frontend.Frontend) (Exposing loc Desugared)

instance PhaseCoerce (Exposition loc Frontend.Frontend) (Exposition loc Desugared)

instance PhaseCoerce (Import loc Frontend.Frontend) (Import loc Desugared)

instance PhaseCoerce (Import' loc Frontend.Frontend) (Import' loc Desugared)

instance PhaseCoerce (ImportExposingOrHiding loc Frontend.Frontend) (ImportExposingOrHiding loc Desugared)

type Desugar a = Eff DesugarPipelineEffects a

type DesugarPipelineEffects = '[Eff.State DesugarState, Eff.Error DesugarError, StructuredDebug]

newtype DesugarState = DesugarState
    { _partialDeclarations :: Map Name PartialDeclaration
    }
    deriving (Monoid, Pretty, Semigroup, Show)

makeLenses ''DesugarState

getDesugaredModule ::
    (QueryEffects es, Rock.Rock Elara.Query.Query :> es) =>
    ModuleName ->
    Eff
        es
        (Module SourceRegion Desugared)
getDesugaredModule mn = runErrorAsElaraError @DesugarError $ do
    parsed <- Rock.fetch $ Elara.Query.ParsedModule mn
    inject $ Eff.evalState (DesugarState mempty) $ desugar parsed

desugar ::
    Module SourceRegion Frontend.Frontend ->
    Desugar (Module SourceRegion Desugared)
desugar (Module loc (Module' name exposing imports decls)) = do
    decls' <- desugarDeclarations name decls
    pure (Module loc (Module' name (phaseCoerce exposing) (map phaseCoerce imports) decls'))

desugarDeclarations ::
    TaggedLocate ModuleNode SourceRegion ModuleName ->
    [Declaration SourceRegion Frontend.Frontend] ->
    Desugar [Declaration SourceRegion Desugared]
desugarDeclarations mn decls = do
    genPartials decls
    completePartials mn

assertPartialNamesEqual :: (PartialDeclaration, TaggedLocate VarNode SourceRegion VarName) -> (PartialDeclaration, TaggedLocate VarNode SourceRegion VarName) -> Desugar ()
assertPartialNamesEqual (p1, n1) (p2, n2) = if n1 ^. unlocated == n2 ^. unlocated then pass else throwError (PartialNamesNotEqual p1 p2)

mergeAnnotations :: Maybe [Annotation SourceRegion Desugared] -> Maybe [Annotation SourceRegion Desugared] -> Desugar [Annotation SourceRegion Desugared]
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

resolvePartialDeclaration :: PartialDeclaration -> Desugar (DeclarationBody SourceRegion Desugared)
resolvePartialDeclaration (JustDef _ _ ty _) = throwError (DefWithoutLet ty)
resolvePartialDeclaration (JustLet n sr e mAnn) =
    pure (DeclarationBody sr (ValueDeclaration n e () () Nothing (fromMaybe [] mAnn)))
resolvePartialDeclaration (AllDecl n sr ty e ann) =
    pure (DeclarationBody sr (ValueDeclaration n e () () (Just ty) ann))
resolvePartialDeclaration (Immediate (Declaration _ (Declaration' _ body))) =
    pure body

genPartials :: [Declaration SourceRegion Frontend.Frontend] -> Desugar ()
genPartials = traverse_ genPartial
  where
    genPartial :: Declaration SourceRegion Frontend.Frontend -> Desugar ()
    genPartial (Declaration wholeDeclRegion (Declaration' mn (DeclarationBody bodyLoc body))) =
        genPartial' body
      where
        genPartial' :: DeclarationBody' SourceRegion Frontend.Frontend -> Desugar ()
        genPartial' db = do
            partial <- genPartial'' db
            let name = declBodyName db
            let f = insertWithM mergePartials (name ^. unlocated) partial
            let modifyM g = Eff.get >>= (Eff.put <=< g)
            modifyM (traverseOf partialDeclarations f)

        genPartial'' :: DeclarationBody' SourceRegion Frontend.Frontend -> Desugar PartialDeclaration
        genPartial'' (ValueDeclaration n e pats _mTy _meta annotations) = do
            e' <- desugarExpr e
            pats' <- traverse desugarPattern pats
            let body' = foldLambda pats' e'
            ann <- traverse desugarAnnotation annotations
            pure (JustLet n wholeDeclRegion body' (Just ann))
        genPartial'' (DeclBodyExtension (Frontend.FrontendValueTypeDef n ty annotations)) = do
            ty' <- desugarType ty
            ann <- traverse desugarAnnotation annotations
            pure (JustDef n wholeDeclRegion ty' (Just ann))
        genPartial'' (TypeDeclarationBody n vars typeDecl _mKind _meta annotations) = do
            typeDecl' <- desugarTypeDeclaration typeDecl
            ann <- traverse desugarAnnotation annotations
            let decl' = TypeDeclarationBody n vars typeDecl' Nothing NoExtension ann
            logDebug $
                "Desugared type declaration at "
                    <> pretty bodyLoc
                    <> " for type "
                    <> pretty (n ^. unlocated)

            let overallLocation = getLocation n <.> bodyLoc
            let finalBody = DeclarationBody bodyLoc decl'
            let finalDecl = Declaration overallLocation (Declaration' mn finalBody)
            pure (Immediate finalDecl)

        declBodyName :: DeclarationBody' SourceRegion Frontend.Frontend -> TaggedLocate VarNode SourceRegion Name
        declBodyName (ValueDeclaration n _ _ _ _ _) = fmap toName n
        declBodyName (TypeDeclarationBody n _ _ _ _ _) = NameType <$> retag VarNode n
        declBodyName (DeclBodyExtension (Frontend.FrontendValueTypeDef n _ _)) = fmap toName n

completePartials :: TaggedLocate ModuleNode SourceRegion ModuleName -> Desugar [Declaration SourceRegion Desugared]
completePartials mn = do
    partials <- use' partialDeclarations
    traverse (completePartial mn) (M.elems partials)

completePartial ::
    TaggedLocate ModuleNode SourceRegion ModuleName ->
    PartialDeclaration ->
    Desugar (Declaration SourceRegion Desugared)
completePartial _mn (Immediate decl) = pure decl
completePartial mn partial = do
    body <- resolvePartialDeclaration partial

    let locatedName = getPartialName partial
    let declaration' = Declaration' mn body
    let DeclarationBody bodyLoc _ = body

    logDebug $
        "Merging locations for declaration " <> pretty (locatedName ^. unlocated)
    let overallLocation = getLocation locatedName <.> bodyLoc

    pure (Declaration overallLocation declaration')

desugarAnnotation :: Annotation SourceRegion Frontend.Frontend -> Desugar (Annotation SourceRegion Desugared)
desugarAnnotation (Annotation n args) = Annotation n <$> traverse desugarAnnotationArg args

desugarAnnotationArg :: AnnotationArg SourceRegion Frontend.Frontend -> Desugar (AnnotationArg SourceRegion Desugared)
desugarAnnotationArg (AnnotationArg e) = AnnotationArg <$> desugarExpr e

desugarTypeDeclaration :: TypeDeclaration SourceRegion Frontend.Frontend -> Desugar (TypeDeclaration SourceRegion Desugared)
desugarTypeDeclaration (Alias t) = Alias <$> desugarType t
desugarTypeDeclaration (ADT constructors) = ADT <$> traverse (\(cn, tys) -> (cn,) <$> traverse desugarType tys) constructors

desugarType :: Type SourceRegion Frontend.Frontend -> Desugar (Type SourceRegion Desugared)
desugarType (Type loc () t') = Type loc () <$> desugarType' t'
  where
    desugarType' :: Type' SourceRegion Frontend.Frontend -> Desugar (Type' SourceRegion Desugared)
    desugarType' (TVar n) = pure (TVar n)
    desugarType' (TFun t1 t2) = TFun <$> desugarType t1 <*> desugarType t2
    desugarType' TUnit = pure TUnit
    desugarType' (TApp t1 t2) = TApp <$> desugarType t1 <*> desugarType t2
    desugarType' (TUserDefined n) = pure (TUserDefined n)
    desugarType' (TRecord fields) = TRecord <$> traverse (\(n, t) -> (n,) <$> desugarType t) fields
    desugarType' (TList t) = TList <$> desugarType t
    desugarType' (TExtension (TupleType ts)) = TExtension . TupleType <$> traverse desugarType ts

desugarExpr :: Expr SourceRegion Frontend.Frontend -> Desugar (Expr SourceRegion Desugared)
desugarExpr (Expr loc () e') = Expr loc () <$> desugarExpr' e'
  where
    desugarExpr' :: Expr' SourceRegion Frontend.Frontend -> Desugar (Expr' SourceRegion Desugared)
    desugarExpr' (EInt i) = pure (EInt i)
    desugarExpr' (EFloat f) = pure (EFloat f)
    desugarExpr' (EString s) = pure (EString s)
    desugarExpr' (EChar c) = pure (EChar c)
    desugarExpr' EUnit = pure EUnit
    desugarExpr' (EVar NoExtension v) = pure (EVar NoExtension v)
    desugarExpr' (ECon NoExtension c) = pure (ECon NoExtension c)
    desugarExpr' (ELam NoExtension pat body) = do
        pat' <- desugarPattern pat
        body' <- desugarExpr body
        pure (ELam NoExtension pat' body')
    desugarExpr' (EApp NoExtension e1 e2) = EApp NoExtension <$> desugarExpr e1 <*> desugarExpr e2
    desugarExpr' (ETyApp e1 ty) = ETyApp <$> desugarExpr e1 <*> desugarType ty
    desugarExpr' (EIf a b c) = EIf <$> desugarExpr a <*> desugarExpr b <*> desugarExpr c
    desugarExpr' (EMatch e cases) = do
        e'' <- desugarExpr e
        cases' <- traverse (bitraverse desugarPattern desugarExpr) cases
        pure (EMatch e'' cases')
    desugarExpr' (ELetIn NoExtension n e body) =
        ELetIn NoExtension n <$> desugarExpr e <*> desugarExpr body
    desugarExpr' (EAnn e ty) = EAnn <$> desugarExpr e <*> desugarType ty
    desugarExpr' (EExtension ext) = desugarExprExtension loc ext

desugarExprExtension ::
    -- | Location of the whole expression, used for error reporting
    NodeLoc ExprNode SourceRegion ->
    Frontend.FrontendExpressionExtension SourceRegion ->
    Desugar (Expr' SourceRegion Desugared)
desugarExprExtension _ (Frontend.FrontendMultiLam pats body) = do
    pats' <- traverse desugarPattern pats
    body' <- desugarExpr body
    let folded = foldLambda pats' body'
    let Expr _ () inner = folded
    pure inner
desugarExprExtension _ (Frontend.FrontendLetInWithPatterns n pats e body) = do
    pats' <- traverse desugarPattern pats
    e' <- desugarExpr e
    body' <- desugarExpr body
    pure (ELetIn NoExtension n (foldLambda pats' e') body')
desugarExprExtension _ (Frontend.FrontendBinaryOperator ext) = do
    ext' <- desugarBinaryOperatorExtension ext
    pure (EExtension (DesugaredBinaryOperator ext'))
desugarExprExtension _ (Frontend.FrontendInParens (InParensExpression e)) = do
    e' <- desugarExpr e
    pure (EExtension (DesugaredInParens (InParensExpression e')))
desugarExprExtension _ (Frontend.FrontendList (ListExpression es)) = do
    es' <- traverse desugarExpr es
    pure (EExtension (DesugaredList (ListExpression es')))
desugarExprExtension _ (Frontend.FrontendTuple (TupleExpression es)) = do
    es' <- traverse desugarExpr es
    pure (EExtension (DesugaredTuple (TupleExpression es')))
desugarExprExtension _ (Frontend.FrontendLetBangIn name val body) = do
    val' <- desugarExpr val
    body' <- desugarExpr body
    pure (EExtension (DesugaredLetBangIn name val' body'))
desugarExprExtension blockLoc (Frontend.FrontendBlock stmts) = do
    stmts' <- traverse desugarStatement stmts
    case last stmts' of
        Statement _ (SExpr final) ->
            case nonEmpty (init stmts') of
                Nothing -> let Expr _ () inner = final in pure inner -- if the block is just 1 expression, replace with just that expression
                Just leading -> pure (EExtension (DesugaredBlock leading final)) -- all leading statements and the final expression
        Statement letLoc SLet{} -> throwError (BlockEndsWithLet PlainLet letLoc blockLoc)
        Statement letLoc SLetBang{} -> throwError (BlockEndsWithLet LetBang letLoc blockLoc)

desugarStatement :: Statement SourceRegion Frontend.Frontend -> Desugar (Statement SourceRegion Desugared)
desugarStatement (Statement loc stmt) = Statement loc <$> desugarStatement' stmt
  where
    desugarStatement' :: Statement' SourceRegion Frontend.Frontend -> Desugar (Statement' SourceRegion Desugared)
    desugarStatement' (SLet n pats e) = do
        pats' <- traverse desugarPattern pats
        e' <- desugarExpr e
        pure (SLet n () (foldLambda pats' e'))
    desugarStatement' (SLetBang n e) = SLetBang n <$> desugarExpr e
    desugarStatement' (SExpr e) = SExpr <$> desugarExpr e

desugarBinaryOperatorExtension :: BinaryOperatorExtension SourceRegion Frontend.Frontend -> Desugar (BinaryOperatorExtension SourceRegion Desugared)
desugarBinaryOperatorExtension (BinaryOperatorExpression op a b) = do
    op' <- desugarBinaryOperator op
    a' <- desugarExpr a
    b' <- desugarExpr b
    pure (BinaryOperatorExpression op' a' b')

desugarBinaryOperator :: BinaryOperator SourceRegion Frontend.Frontend -> Desugar (BinaryOperator SourceRegion Desugared)
desugarBinaryOperator (SymOp loc occ) = pure (SymOp loc occ)
desugarBinaryOperator (InfixedOp loc occ) = pure (InfixedOp loc occ)

desugarPattern :: Pattern SourceRegion Frontend.Frontend -> Desugar (Pattern SourceRegion Desugared)
desugarPattern p@(Pattern loc meta p') = do
    meta' <- traverse desugarType meta
    p'' <- desugarPattern' p'
    pure (Pattern loc meta' p'')
  where
    desugarPattern' :: Pattern' SourceRegion Frontend.Frontend -> Desugar (Pattern' SourceRegion Desugared)
    desugarPattern' (PVar v) = pure (PVar v)
    desugarPattern' (PCon c pats) = PCon c <$> traverse desugarPattern pats
    desugarPattern' PWildcard = pure PWildcard
    desugarPattern' (PInt i) = pure (PInt i)
    desugarPattern' (PFloat f) = pure (PFloat f)
    desugarPattern' (PString s) = pure (PString s)
    desugarPattern' (PChar c) = pure (PChar c)
    desugarPattern' PUnit = pure PUnit
    desugarPattern' (PExtension ext) = desugarPatternExtension p ext

    desugarPatternExtension :: Pattern SourceRegion Frontend.Frontend -> ListTuplePatternExtension SourceRegion Frontend.Frontend -> Desugar (Pattern' SourceRegion Desugared)
    desugarPatternExtension _ (ListPattern pats) = PExtension . ListPattern <$> traverse desugarPattern pats
    desugarPatternExtension _ (ConsPattern l r) = (\l' r' -> PExtension (ConsPattern l' r')) <$> desugarPattern l <*> desugarPattern r
    desugarPatternExtension p (TuplePattern (_ :| [])) = throwError (TuplePatternTooShort p)
    desugarPatternExtension _ (TuplePattern pats) = do
        pats' <- traverse desugarPattern pats
        pure (PExtension (TuplePattern pats'))

{- | Lambdas need quite a lot of desugaring - they need to be unfolded into a series of nested lambdas, and then each pattern needs to be converted into a match expression.

For example, @\a (b, c) 1 -> e@ becomes
@\a -> \b_1 -> \c_1 -> match b_1 with
                          (b, c) -> match 1 with
                                      1 -> e@

However, converting the matches would require renaming, and we're not able to do that yet.
Instead, we unfold the lambda, but keep the patterns, and the renamer handles the match conversion.
-}
foldLambda :: [Pattern SourceRegion Desugared] -> Expr SourceRegion Desugared -> Expr SourceRegion Desugared
foldLambda [] e = e
foldLambda (p@(Pattern pLoc _ _) : ps) e =
    let inner@(Expr innerLoc _ _) = foldLambda ps e
        newLoc = pLoc <.> innerLoc
     in Expr newLoc () (ELam NoExtension p inner)
