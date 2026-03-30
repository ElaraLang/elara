{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Round-trip pretty-printing for Frontend AST.
Pretty instances here produce fully-parenthesized output suitable for
property tests that check @parse . pretty == id@.
These are necessarily orphan instances (Frontend defined in parent module,
Pretty class defined in Elara.Data.Pretty).
-}
module Elara.AST.Phases.Frontend.Pretty (
    prettyExprRoundTrip,
    prettyPatternRoundTrip,
) where

import Elara.AST.Extensions
import Elara.AST.Phase ()
import Elara.AST.Phases.Frontend
import Elara.AST.Pretty
import Elara.AST.Types
import Elara.Data.AtLeast2List qualified as AtLeast2List
import Elara.Data.Pretty (Pretty (..), escapeChar)
import Prettyprinter hiding (Pretty (..))
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prelude hiding (group)

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
