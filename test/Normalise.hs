{-# OPTIONS_GHC -Wno-orphans #-}

{- | Utilities for testing with new AST types.
Provides location stripping (replacing all SourceRegion values with ())
and smart constructors for building unlocated test AST values.
-}
module Normalise (
    -- * Location stripping
    stripExpr,
    stripPattern,
    stripType,
    stripBinaryOperator,

    -- * Smart constructors (unlocated)
    mkExpr,
    mkPat,
    mkType,

    -- * Stripping InParens for new types
    stripNewInParens,
)
where

import Elara.AST.Extensions
import Elara.AST.Phases.Frontend
import Elara.AST.Region (SourceRegion, unlocated)
import Elara.AST.Types
import Relude.Extra (bimapF)

mkExpr :: Expr' () Frontend -> Expr () Frontend
mkExpr = Expr () ()

mkPat :: Pattern' () Frontend -> Pattern () Frontend
mkPat = Pattern () Nothing

mkType :: Type' () Frontend -> Type () Frontend
mkType = Type () ()

stripExpr :: Expr SourceRegion Frontend -> Expr () Frontend
stripExpr (Expr _ meta e') = Expr () meta (stripExpr' e')

stripExpr' :: Expr' SourceRegion Frontend -> Expr' () Frontend
stripExpr' = \case
    EInt i -> EInt i
    EFloat f -> EFloat f
    EString s -> EString s
    EChar c -> EChar c
    EUnit -> EUnit
    EVar ext v -> EVar ext (view unlocated v)
    ECon ext c -> ECon ext (view unlocated c)
    ELam ext pat body -> ELam ext (stripPattern pat) (stripExpr body)
    EApp ext e1 e2 -> EApp ext (stripExpr e1) (stripExpr e2)
    ETyApp e t -> ETyApp (stripExpr e) (stripType t)
    EIf a b c -> EIf (stripExpr a) (stripExpr b) (stripExpr c)
    EMatch e cases -> EMatch (stripExpr e) (map (bimap stripPattern stripExpr) cases)
    ELetIn ext n e body -> ELetIn ext (view unlocated n) (stripExpr e) (stripExpr body)
    ELet ext n e -> ELet ext (view unlocated n) (stripExpr e)
    EBlock es -> EBlock (fmap stripExpr es)
    EAnn e t -> EAnn (stripExpr e) (stripType t)
    EExtension ext -> EExtension (stripExprExt ext)

stripExprExt :: FrontendExpressionExtension SourceRegion -> FrontendExpressionExtension ()
stripExprExt = \case
    FrontendMultiLam pats body -> FrontendMultiLam (map stripPattern pats) (stripExpr body)
    FrontendLetWithPatterns n pats e -> FrontendLetWithPatterns (view unlocated n) (map stripPattern pats) (stripExpr e)
    FrontendLetInWithPatterns n pats e body -> FrontendLetInWithPatterns (view unlocated n) (map stripPattern pats) (stripExpr e) (stripExpr body)
    FrontendBinaryOperator (BinaryOperatorExpression op a b) ->
        FrontendBinaryOperator (BinaryOperatorExpression (stripBinaryOperator op) (stripExpr a) (stripExpr b))
    FrontendInParens (InParensExpression e) -> FrontendInParens (InParensExpression (stripExpr e))
    FrontendList (ListExpression es) -> FrontendList (ListExpression (map stripExpr es))
    FrontendTuple (TupleExpression es) -> FrontendTuple (TupleExpression (fmap stripExpr es))

stripPattern :: Pattern SourceRegion Frontend -> Pattern () Frontend
stripPattern (Pattern _ meta p') = Pattern () (fmap stripType meta) (stripPattern' p')

stripPattern' :: Pattern' SourceRegion Frontend -> Pattern' () Frontend
stripPattern' = \case
    PVar v -> PVar (view unlocated v)
    PCon c pats -> PCon (view unlocated c) (map stripPattern pats)
    PWildcard -> PWildcard
    PInt i -> PInt i
    PFloat f -> PFloat f
    PString s -> PString s
    PChar c -> PChar c
    PUnit -> PUnit
    PExtension ext -> PExtension (stripPatternExt ext)

stripPatternExt :: ListTuplePatternExtension SourceRegion Frontend -> ListTuplePatternExtension () Frontend
stripPatternExt = \case
    ListPattern pats -> ListPattern (map stripPattern pats)
    TuplePattern pats -> TuplePattern (fmap stripPattern pats)
    ConsPattern l r -> ConsPattern (stripPattern l) (stripPattern r)

stripType :: Type SourceRegion Frontend -> Type () Frontend
stripType (Type _ meta t') = Type () meta (stripType' t')

stripType' :: Type' SourceRegion Frontend -> Type' () Frontend
stripType' = \case
    TVar v -> TVar $ v ^. unlocated
    TFun a b -> TFun (stripType a) (stripType b)
    TUnit -> TUnit
    TApp a b -> TApp (stripType a) (stripType b)
    TUserDefined n -> TUserDefined $ n ^. unlocated
    TRecord fields -> TRecord (bimapF (view unlocated) stripType fields)
    TList t -> TList (stripType t)
    TExtension (TupleType ts) -> TExtension (TupleType (fmap stripType ts))

stripBinaryOperator :: BinaryOperator SourceRegion Frontend -> BinaryOperator () Frontend
stripBinaryOperator = \case
    SymOp _ occ -> SymOp () (view unlocated occ)
    InfixedOp _ occ -> InfixedOp () (view unlocated occ)

stripNewInParens :: Expr SourceRegion Frontend -> Expr SourceRegion Frontend
stripNewInParens = go
  where
    go (Expr loc meta e') = Expr loc meta (goE e')

    goE = \case
        EExtension (FrontendInParens (InParensExpression inner)) ->
            let Expr _ _ e' = go inner in e'
        ELam ext p body -> ELam ext (goP p) (go body)
        EApp ext e1 e2 -> EApp ext (go e1) (go e2)
        ETyApp e t -> ETyApp (go e) t
        EIf a b c -> EIf (go a) (go b) (go c)
        EMatch e cases -> EMatch (go e) (map (bimap goP go) cases)
        ELetIn ext n e body -> ELetIn ext n (go e) (go body)
        ELet ext n e -> ELet ext n (go e)
        EBlock es -> EBlock (fmap go es)
        EAnn e t -> EAnn (go e) t
        EExtension ext -> EExtension (goExt ext)
        other -> other

    goExt = \case
        FrontendMultiLam pats body -> FrontendMultiLam (map goP pats) (go body)
        FrontendLetWithPatterns n pats e -> FrontendLetWithPatterns n (map goP pats) (go e)
        FrontendLetInWithPatterns n pats e body -> FrontendLetInWithPatterns n (map goP pats) (go e) (go body)
        FrontendBinaryOperator (BinaryOperatorExpression op a b) ->
            FrontendBinaryOperator (BinaryOperatorExpression op (go a) (go b))
        FrontendInParens (InParensExpression e) -> FrontendInParens (InParensExpression (go e))
        FrontendList (ListExpression es) -> FrontendList (ListExpression (map go es))
        FrontendTuple (TupleExpression es) -> FrontendTuple (TupleExpression (fmap go es))

    goP (Pattern loc meta p') = Pattern loc meta (goP' p')
    goP' = \case
        PCon c pats -> PCon c (map goP pats)
        PExtension ext -> PExtension (goPExt ext)
        other -> other
    goPExt = \case
        ListPattern pats -> ListPattern (map goP pats)
        TuplePattern pats -> TuplePattern (fmap goP pats)
        ConsPattern l r -> ConsPattern (goP l) (goP r)
