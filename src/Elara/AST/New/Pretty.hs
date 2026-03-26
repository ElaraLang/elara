{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.New.Pretty where

import Elara.AST.Name (LowerAlphaName)
import Elara.AST.New.Phase
import Elara.AST.New.Types
import Elara.Data.Pretty (Pretty (..))
import Prettyprinter (Doc, hsep, indent, line, parens, punctuate, vsep, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prelude hiding (group)

-- | Constraint alias for all the Pretty constraints needed at a given loc
type PrettyPhaseLoc p loc =
    ( Pretty (ValueOccurrence p loc)
    , Pretty (ConstructorOccurrence p loc)
    , Pretty (TypeOccurrence p loc)
    , Pretty (OperatorOccurrence p loc)
    , Pretty (InfixedOccurrence p loc)
    , Pretty (ValueBinder p loc)
    , Pretty (TopValueBinder p loc)
    , Pretty (TopTypeBinder p loc)
    , Pretty (TypeVariable p loc)
    , Pretty (ConstructorBinder p loc)
    , Pretty (Locate loc LowerAlphaName)
    )

{- | Phase-specific pretty-printing. Each phase provides ONE instance.
This replaces ~25 separate Pretty constraints per instance.
-}
class PrettyPhase p where
    prettyValueOccurrence :: forall loc. PrettyPhaseLoc p loc => ValueOccurrence p loc -> Doc AnsiStyle
    prettyConstructorOccurrence :: forall loc. PrettyPhaseLoc p loc => ConstructorOccurrence p loc -> Doc AnsiStyle
    prettyTypeOccurrence :: forall loc. PrettyPhaseLoc p loc => TypeOccurrence p loc -> Doc AnsiStyle
    prettyOperatorOccurrence :: forall loc. PrettyPhaseLoc p loc => OperatorOccurrence p loc -> Doc AnsiStyle
    prettyInfixedOccurrence :: forall loc. PrettyPhaseLoc p loc => InfixedOccurrence p loc -> Doc AnsiStyle
    prettyValueBinder :: forall loc. PrettyPhaseLoc p loc => ValueBinder p loc -> Doc AnsiStyle
    prettyTopValueBinder :: forall loc. PrettyPhaseLoc p loc => TopValueBinder p loc -> Doc AnsiStyle
    prettyTopTypeBinder :: forall loc. PrettyPhaseLoc p loc => TopTypeBinder p loc -> Doc AnsiStyle
    prettyTypeVariable :: forall loc. PrettyPhaseLoc p loc => TypeVariable p loc -> Doc AnsiStyle
    prettyConstructorBinder :: forall loc. PrettyPhaseLoc p loc => ConstructorBinder p loc -> Doc AnsiStyle
    prettyLambdaBinder :: forall loc. (PrettyPhaseLoc p loc, PrettyExtensions p) => LambdaBinder p loc -> Doc AnsiStyle
    prettyExpressionMeta :: forall loc. ExpressionMeta p loc -> Maybe (Doc AnsiStyle)
    prettyPatternMeta :: forall loc. (PrettyPhaseLoc p loc, PrettyExtensions p) => PatternMeta p loc -> Maybe (Doc AnsiStyle)
    prettyTypeMeta :: forall loc. TypeMeta p loc -> Maybe (Doc AnsiStyle)

-- | Pretty-printing for phase-specific extension constructors
class PrettyExtensions p where
    prettyExpressionExtension :: forall loc. (PrettyPhase p, PrettyPhaseLoc p loc) => ExpressionExtension p loc -> Doc AnsiStyle
    prettyPatternExtension :: forall loc. (PrettyPhase p, PrettyPhaseLoc p loc) => PatternExtension p loc -> Doc AnsiStyle
    prettyTypeSyntaxExtension :: forall loc. (PrettyPhase p, PrettyPhaseLoc p loc) => TypeSyntaxExtension p loc -> Doc AnsiStyle
    prettyDeclBodyExtension :: forall loc. (PrettyPhase p, PrettyPhaseLoc p loc) => DeclBodyExtension p loc -> Doc AnsiStyle

-- | Pretty-print an expression. Only 2 constraints!
prettyExpr :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => Expr loc p -> Doc AnsiStyle
prettyExpr (Expr _ meta node) =
    let pe = prettyExpr' @loc @p node
        te = prettyExpressionMeta @p @loc meta
     in case te of
            Nothing -> pe
            Just t -> pe <+> ":" <+> t

prettyExpr' :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => Expr' loc p -> Doc AnsiStyle
prettyExpr' = \case
    EInt i -> pretty i
    EFloat f -> pretty f
    EString s -> "\"" <> pretty s <> "\""
    EChar c -> "'" <> pretty (show c) <> "'"
    EUnit -> "()"
    EVar _ v -> prettyValueOccurrence @p @loc v
    ECon _ c -> prettyConstructorOccurrence @p @loc c
    ELam _ binder body ->
        "\\" <> prettyLambdaBinder @p @loc binder <+> "->" <+> prettyExpr body
    EApp _ f x -> prettyExpr f <+> prettyExpr x
    ETyApp e t -> prettyExpr e <+> "@" <> parens (prettyType t)
    EIf cond then_ else_ ->
        "if" <+> prettyExpr cond <+> "then" <+> prettyExpr then_ <+> "else" <+> prettyExpr else_
    EMatch scrut alts ->
        "match"
            <+> prettyExpr scrut
            <+> "with"
            <> line
            <> indent 4 (vsep (map prettyAlt alts))
    ELetIn _ binder val body ->
        "let" <+> prettyValueBinder @p @loc binder <+> "=" <+> prettyExpr val <+> "in" <+> prettyExpr body
    ELet _ binder val ->
        "let" <+> prettyValueBinder @p @loc binder <+> "=" <+> prettyExpr val
    EBlock exprs -> vsep (toList (prettyExpr <$> exprs))
    EAnn e t -> prettyExpr e <+> ":" <+> prettyType t
    EExtension ext -> prettyExpressionExtension @p @loc ext
  where
    prettyAlt (pat, body) = prettyPattern pat <+> "->" <+> prettyExpr body

-- | Pretty-print a pattern
prettyPattern :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => Pattern loc p -> Doc AnsiStyle
prettyPattern (Pattern _ meta node) =
    let pp = prettyPattern' @loc @p node
        te = prettyPatternMeta @p @loc meta
     in case te of
            Nothing -> pp
            Just t -> pp <+> ":" <+> t

prettyPattern' :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => Pattern' loc p -> Doc AnsiStyle
prettyPattern' = \case
    PVar v -> prettyValueBinder @p @loc v
    PCon c ps -> prettyConstructorOccurrence @p @loc c <+> hsep (map prettyPattern ps)
    PWildcard -> "_"
    PInt i -> pretty i
    PFloat f -> pretty f
    PString s -> "\"" <> pretty s <> "\""
    PChar c -> "'" <> pretty (show c) <> "'"
    PUnit -> "()"
    PExtension ext -> prettyPatternExtension @p @loc ext

-- | Pretty-print a type
prettyType :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => Type loc p -> Doc AnsiStyle
prettyType (Type _ meta node) =
    let pt = prettyType' @loc @p node
        te = prettyTypeMeta @p @loc meta
     in case te of
            Nothing -> pt
            Just k -> pt <+> ":" <+> k

prettyType' :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => Type' loc p -> Doc AnsiStyle
prettyType' = \case
    TVar v -> prettyTypeVariable @p @loc v
    TFun a b -> parens (prettyType a <+> "->" <+> prettyType b)
    TUnit -> "()"
    TApp f x -> prettyType f <+> prettyType x
    TUserDefined name -> prettyTypeOccurrence @p @loc name
    TRecord fields -> "{" <+> hsep (punctuate "," (map prettyField (toList fields))) <+> "}"
      where
        prettyField :: (Locate loc LowerAlphaName, Type loc p) -> Doc AnsiStyle
        prettyField (name, ty) = pretty name <+> ":" <+> prettyType ty
    TList t -> "[" <+> prettyType t <+> "]"
    TExtension ext -> prettyTypeSyntaxExtension @p @loc ext

-- | Pretty-print a binary operator
prettyBinaryOperator :: forall loc p. (PrettyPhase p, PrettyPhaseLoc p loc) => BinaryOperator loc p -> Doc AnsiStyle
prettyBinaryOperator = \case
    SymOp _ op -> prettyOperatorOccurrence @p @loc op
    InfixedOp _ name -> "`" <> prettyInfixedOccurrence @p @loc name <> "`"
