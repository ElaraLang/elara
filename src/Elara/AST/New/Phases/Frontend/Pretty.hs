{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Elara.AST.New.Phases.Frontend.Pretty where

import Elara.AST.New.Extensions
import Elara.AST.New.Phase ()
import Elara.AST.New.Phases.Frontend
import Elara.AST.New.Pretty
import Elara.AST.New.Types
import Elara.Data.AtLeast2List qualified as AtLeast2List
import Elara.Data.Pretty (Pretty (..), escapeChar)
import Prettyprinter hiding (Pretty (..))
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prelude hiding (group)

instance PrettyPhase Frontend where
    prettyValueOccurrence = pretty
    prettyConstructorOccurrence = pretty
    prettyTypeOccurrence = pretty
    prettyOperatorOccurrence = pretty
    prettyInfixedOccurrence = pretty
    prettyValueBinder = pretty
    prettyTopValueBinder = pretty
    prettyTopTypeBinder = pretty
    prettyTypeVariable = pretty
    prettyConstructorBinder = pretty
    prettyLambdaBinder = prettyPattern
    prettyExpressionMeta () = Nothing
    prettyPatternMeta Nothing = Nothing
    prettyPatternMeta (Just t) = Just (prettyType t)
    prettyTypeMeta () = Nothing

instance PrettyExtensions Frontend where
    prettyExpressionExtension = prettyFrontendExprExt
    prettyPatternExtension = prettyListTuplePatternExt
    prettyTypeSyntaxExtension = prettyTupleTypeExt
    prettyDeclBodyExtension = prettyFrontendDeclBodyExt

prettyFrontendExprExt :: forall loc. PrettyPhaseLoc Frontend loc => FrontendExpressionExtension loc -> Doc AnsiStyle
prettyFrontendExprExt = \case
    FrontendMultiLam pats body ->
        parens ("\\" <> hsep (map prettyPattern pats) <+> "->" <+> parens (prettyExpr body))
    FrontendLetWithPatterns binder pats val ->
        "let" <+> pretty binder <+> hsep (map prettyPattern pats) <+> "=" <+> prettyExpr val
    FrontendLetInWithPatterns binder pats val body ->
        "let" <+> pretty binder <+> hsep (map prettyPattern pats) <+> "=" <+> prettyExpr val <+> "in" <+> prettyExpr body
    FrontendBinaryOperator (BinaryOperatorExpression op lhs rhs) ->
        parens (prettyExpr lhs <+> prettyBinaryOperator @_ @Frontend op <+> prettyExpr rhs)
    FrontendInParens (InParensExpression e) ->
        parens (prettyExpr e)
    FrontendList (ListExpression es) ->
        "[" <+> hsep (punctuate "," (map prettyExpr es)) <+> "]"
    FrontendTuple (TupleExpression items) ->
        parens (hsep (punctuate "," (map prettyExpr (AtLeast2List.toList items))))

prettyListTuplePatternExt :: forall loc. PrettyPhaseLoc Frontend loc => ListTuplePatternExtension loc Frontend -> Doc AnsiStyle
prettyListTuplePatternExt = \case
    ListPattern ps ->
        "[" <+> hsep (punctuate "," (map prettyPattern ps)) <+> "]"
    TuplePattern ps ->
        parens (hsep (punctuate "," (map prettyPattern (toList ps))))
    ConsPattern hd tl ->
        parens (prettyPattern hd <+> "::" <+> prettyPattern tl)

prettyTupleTypeExt :: forall loc. PrettyPhaseLoc Frontend loc => TupleTypeExtension loc Frontend -> Doc AnsiStyle
prettyTupleTypeExt (TupleType items) =
    parens (hsep (punctuate "," (map prettyType (AtLeast2List.toList items))))

prettyFrontendDeclBodyExt :: forall loc. PrettyPhaseLoc Frontend loc => FrontendDeclBodyExtension loc -> Doc AnsiStyle
prettyFrontendDeclBodyExt (FrontendValueTypeDef name ty _annotations) =
    "def" <+> pretty name <+> ":" <+> prettyType ty

instance PrettyPhaseLoc Frontend loc => Pretty (Expr loc Frontend) where
    pretty = prettyExprRoundTrip

instance PrettyPhaseLoc Frontend loc => Pretty (Pattern loc Frontend) where
    pretty = prettyPatternRoundTrip

instance PrettyPhaseLoc Frontend loc => Pretty (Type loc Frontend) where
    pretty = prettyType

prettyExprRoundTrip :: forall loc. PrettyPhaseLoc Frontend loc => Expr loc Frontend -> Doc AnsiStyle
prettyExprRoundTrip (Expr _ _meta node) = prettyExprRoundTrip' node

prettyExprRoundTrip' :: forall loc. PrettyPhaseLoc Frontend loc => Expr' loc Frontend -> Doc AnsiStyle
prettyExprRoundTrip' = \case
    EInt i -> pretty i
    EFloat f -> pretty f
    EString s -> "\"" <> pretty s <> "\""
    EChar c -> "'" <> pretty (escapeChar @Text c) <> "'"
    EUnit -> "()"
    EVar _ v -> prettyValueOccurrence @Frontend @loc v
    ECon _ c -> prettyConstructorOccurrence @Frontend @loc c
    ELam _ binder body ->
        parens ("\\" <> prettyPatternRoundTrip binder <+> "->" <+> parens (prettyExprRoundTrip body))
    EApp _ f x ->
        parens (prettyExprRoundTrip f <+> prettyExprRoundTrip x)
    ETyApp e t -> prettyExprRoundTrip e <+> "@" <> parens (prettyType t)
    EIf cond then_ else_ ->
        parens ("if" <+> prettyExprRoundTrip cond <+> "then" <+> prettyExprRoundTrip then_ <+> "else" <+> prettyExprRoundTrip else_)
    EMatch scrut alts ->
        parens
            ( "match"
                <+> prettyExprRoundTrip scrut
                <+> "with"
                <+> "{"
                <+> hsep (punctuate ";" (map prettyAlt alts))
                <+> "}"
            )
      where
        prettyAlt (pat, body) = prettyPatternRoundTrip pat <+> "->" <+> prettyExprRoundTrip body
    ELetIn _ binder val body ->
        parens ("let" <+> prettyValueBinder @Frontend @loc binder <+> "=" <+> prettyExprRoundTrip val <+> "in" <+> prettyExprRoundTrip body)
    ELet _ binder val ->
        parens ("let" <+> prettyValueBinder @Frontend @loc binder <+> "=" <+> prettyExprRoundTrip val)
    EBlock exprs ->
        "{" <+> hsep (punctuate ";" (map prettyExprRoundTrip (toList exprs))) <+> "}"
    EAnn e t -> prettyExprRoundTrip e <+> ":" <+> prettyType t
    EExtension ext -> prettyFrontendExprExtRoundTrip ext

prettyFrontendExprExtRoundTrip :: forall loc. PrettyPhaseLoc Frontend loc => FrontendExpressionExtension loc -> Doc AnsiStyle
prettyFrontendExprExtRoundTrip = \case
    FrontendMultiLam pats body ->
        parens ("\\" <> hsep (map prettyPatternRoundTrip pats) <+> "->" <+> parens (prettyExprRoundTrip body))
    FrontendLetWithPatterns binder pats val ->
        parens ("let" <+> pretty binder <+> hsep (map prettyPatternRoundTrip pats) <+> "=" <+> prettyExprRoundTrip val)
    FrontendLetInWithPatterns binder pats val body ->
        parens ("let" <+> pretty binder <+> hsep (map prettyPatternRoundTrip pats) <+> "=" <+> prettyExprRoundTrip val <+> "in" <+> prettyExprRoundTrip body)
    FrontendBinaryOperator (BinaryOperatorExpression op lhs rhs) ->
        parens (prettyExprRoundTrip lhs <+> prettyBinaryOperator @_ @Frontend op <+> prettyExprRoundTrip rhs)
    FrontendInParens (InParensExpression e) ->
        parens (prettyExprRoundTrip e)
    FrontendList (ListExpression es) ->
        "[" <+> hsep (punctuate "," (map prettyExprRoundTrip es)) <+> "]"
    FrontendTuple (TupleExpression items) ->
        parens (hsep (punctuate "," (map prettyExprRoundTrip (AtLeast2List.toList items))))

prettyPatternRoundTrip :: forall loc. PrettyPhaseLoc Frontend loc => Pattern loc Frontend -> Doc AnsiStyle
prettyPatternRoundTrip (Pattern _ meta node) =
    let pp = prettyPatternRoundTrip' node
     in case meta of
            Nothing -> pp
            Just t -> pp <+> ":" <+> prettyType t

prettyPatternRoundTrip' :: forall loc. PrettyPhaseLoc Frontend loc => Pattern' loc Frontend -> Doc AnsiStyle
prettyPatternRoundTrip' = \case
    PVar v -> prettyValueBinder @Frontend @loc v
    PCon c [] -> prettyConstructorOccurrence @Frontend @loc c
    PCon c ps -> parens (prettyConstructorOccurrence @Frontend @loc c <+> hsep (map prettyPatternRoundTrip ps))
    PWildcard -> "_"
    PInt i -> pretty i
    PFloat f -> pretty f
    PString s -> "\"" <> pretty s <> "\""
    PChar c -> "'" <> pretty (escapeChar @Text c) <> "'"
    PUnit -> "()"
    PExtension ext -> prettyListTuplePatternExtRoundTrip ext

prettyListTuplePatternExtRoundTrip :: forall loc. PrettyPhaseLoc Frontend loc => ListTuplePatternExtension loc Frontend -> Doc AnsiStyle
prettyListTuplePatternExtRoundTrip = \case
    ListPattern ps ->
        "[" <+> hsep (punctuate "," (map prettyPatternRoundTrip ps)) <+> "]"
    TuplePattern ps ->
        parens (hsep (punctuate "," (map prettyPatternRoundTrip (toList ps))))
    ConsPattern hd tl ->
        parens (prettyPatternRoundTrip hd <+> "::" <+> prettyPatternRoundTrip tl)
