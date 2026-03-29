{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.New.Pretty where

import Elara.AST.Name (LowerAlphaName, ModuleName)
import Elara.AST.New.Extensions
import Elara.AST.New.Module (Module (..), Module' (..))
import Elara.AST.New.Phase
import Elara.AST.New.Types
import Elara.Data.AtLeast2List qualified as AtLeast2List
import Elara.Data.Pretty (Pretty (..), indentDepth)
import Elara.Pretty.Common (prettyCtorsInline, prettyMatchAlt, prettyMatchAlts)
import Prettyprinter (Doc, flatAlt, group, hsep, indent, line, nest, parens, punctuate, vsep, (<+>))
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

class PrettyPhase p where
    prettyValueOccurrence :: forall loc. PrettyPhaseLoc p loc => ValueOccurrence p loc -> Doc AnsiStyle
    default prettyValueOccurrence :: forall loc. Pretty (ValueOccurrence p loc) => ValueOccurrence p loc -> Doc AnsiStyle
    prettyValueOccurrence = pretty
    prettyConstructorOccurrence :: forall loc. PrettyPhaseLoc p loc => ConstructorOccurrence p loc -> Doc AnsiStyle
    default prettyConstructorOccurrence :: forall loc. Pretty (ConstructorOccurrence p loc) => ConstructorOccurrence p loc -> Doc AnsiStyle
    prettyConstructorOccurrence = pretty
    prettyTypeOccurrence :: forall loc. PrettyPhaseLoc p loc => TypeOccurrence p loc -> Doc AnsiStyle
    default prettyTypeOccurrence :: forall loc. Pretty (TypeOccurrence p loc) => TypeOccurrence p loc -> Doc AnsiStyle
    prettyTypeOccurrence = pretty
    prettyOperatorOccurrence :: forall loc. PrettyPhaseLoc p loc => OperatorOccurrence p loc -> Doc AnsiStyle
    default prettyOperatorOccurrence :: forall loc. Pretty (OperatorOccurrence p loc) => OperatorOccurrence p loc -> Doc AnsiStyle
    prettyOperatorOccurrence = pretty
    prettyInfixedOccurrence :: forall loc. PrettyPhaseLoc p loc => InfixedOccurrence p loc -> Doc AnsiStyle
    default prettyInfixedOccurrence :: forall loc. Pretty (InfixedOccurrence p loc) => InfixedOccurrence p loc -> Doc AnsiStyle
    prettyInfixedOccurrence = pretty
    prettyValueBinder :: forall loc. PrettyPhaseLoc p loc => ValueBinder p loc -> Doc AnsiStyle
    default prettyValueBinder :: forall loc. Pretty (ValueBinder p loc) => ValueBinder p loc -> Doc AnsiStyle
    prettyValueBinder = pretty
    prettyTopValueBinder :: forall loc. PrettyPhaseLoc p loc => TopValueBinder p loc -> Doc AnsiStyle
    default prettyTopValueBinder :: forall loc. Pretty (TopValueBinder p loc) => TopValueBinder p loc -> Doc AnsiStyle
    prettyTopValueBinder = pretty
    prettyTopTypeBinder :: forall loc. PrettyPhaseLoc p loc => TopTypeBinder p loc -> Doc AnsiStyle
    default prettyTopTypeBinder :: forall loc. Pretty (TopTypeBinder p loc) => TopTypeBinder p loc -> Doc AnsiStyle
    prettyTopTypeBinder = pretty
    prettyTypeVariable :: forall loc. PrettyPhaseLoc p loc => TypeVariable p loc -> Doc AnsiStyle
    default prettyTypeVariable :: forall loc. Pretty (TypeVariable p loc) => TypeVariable p loc -> Doc AnsiStyle
    prettyTypeVariable = pretty
    prettyConstructorBinder :: forall loc. PrettyPhaseLoc p loc => ConstructorBinder p loc -> Doc AnsiStyle
    default prettyConstructorBinder :: forall loc. Pretty (ConstructorBinder p loc) => ConstructorBinder p loc -> Doc AnsiStyle
    prettyConstructorBinder = pretty
    prettyLambdaBinder :: forall loc. (PrettyPhaseLoc p loc, PrettyExtensions p) => LambdaBinder p loc -> Doc AnsiStyle
    prettyExpressionMeta :: forall loc. (PrettyPhaseLoc p loc, PrettyExtensions p) => ExpressionMeta p loc -> Maybe (Doc AnsiStyle)
    prettyPatternMeta :: forall loc. (PrettyPhaseLoc p loc, PrettyExtensions p) => PatternMeta p loc -> Maybe (Doc AnsiStyle)
    prettyTypeMeta :: forall loc. TypeMeta p loc -> Maybe (Doc AnsiStyle)
    prettyValueDeclPatterns :: forall loc. (PrettyPhaseLoc p loc, PrettyExtensions p) => ValueDeclPatterns p loc -> Doc AnsiStyle
    prettyValueDeclPatterns _ = mempty

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
    EApp _ f x -> prettyExprAppFun f <+> prettyExprAtom x
    ETyApp e t -> prettyExprAppFun e <+> "@" <> prettyTypeAtom t
    EIf cond then_ else_ ->
        "if" <+> prettyExpr cond <+> "then" <+> prettyExpr then_ <+> "else" <+> prettyExpr else_
    EMatch scrut alts ->
        "match"
            <+> prettyExpr scrut
            <+> "with"
            <> prettyMatchAlts 4 (map prettyAlt alts)
    ELetIn _ binder val body ->
        group
            ( "let"
                <+> prettyValueBinder @p @loc binder
                <+> "="
                <+> prettyExpr val
                <> flatAlt (line <> "in" <+> prettyExpr body) (" in " <> prettyExpr body)
            )
    ELet _ binder val ->
        case val of
            Expr _ _ (EBlock es) ->
                "let"
                    <+> prettyValueBinder @p @loc binder
                    <+> line
                    <> indent 2 (vsep (map prettyExpr (toList es)))
            _ ->
                "let" <+> prettyValueBinder @p @loc binder <+> "=" <+> prettyExpr val
    EBlock exprs -> case exprs of
        x :| [] -> prettyExpr x
        x :| rest -> prettyExpr x <> line <> vsep (map prettyExpr rest)
    EAnn e t -> prettyExpr e <+> ":" <+> prettyType t
    EExtension ext -> prettyExpressionExtension @p @loc ext
  where
    prettyAlt (pat, body) = prettyMatchAlt (prettyPattern pat) (prettyExpr body)

prettyExprAtom :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => Expr loc p -> Doc AnsiStyle
prettyExprAtom e@(Expr _ _ node) = case node of
    EVar{} -> prettyExpr e
    ECon{} -> prettyExpr e
    EInt{} -> prettyExpr e
    EFloat{} -> prettyExpr e
    EString{} -> prettyExpr e
    EChar{} -> prettyExpr e
    EUnit{} -> prettyExpr e
    _ -> parens (prettyExpr e)

prettyExprAppFun :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => Expr loc p -> Doc AnsiStyle
prettyExprAppFun e@(Expr _ _ node) = case node of
    EVar{} -> prettyExpr e
    ECon{} -> prettyExpr e
    EApp{} -> prettyExpr e
    EInt{} -> prettyExpr e
    EFloat{} -> prettyExpr e
    EString{} -> prettyExpr e
    EChar{} -> prettyExpr e
    EUnit{} -> prettyExpr e
    _ -> parens (prettyExpr e)

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
    TFun a b -> prettyTypeAtom a <+> "->" <+> prettyType b
    TUnit -> "()"
    TApp f x -> prettyTypeAppFun f <+> prettyTypeAtom x
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

-- | Pretty-print a type declaration (ADT or alias)
prettyTypeDeclaration :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => TypeDeclaration loc p -> Doc AnsiStyle
prettyTypeDeclaration = \case
    ADT ctors ->
        let prettyCtor (name, args) =
                prettyConstructorBinder @p @loc name <> case args of
                    [] -> mempty
                    _ -> " " <> hsep (map prettyTypeAtom args)
         in prettyCtorsInline (map prettyCtor (toList ctors))
    Alias t -> prettyType t

-- | Pretty-print a type in "atom" position (parenthesized if compound)
prettyTypeAtom :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => Type loc p -> Doc AnsiStyle
prettyTypeAtom t@(Type _ _ node) = case node of
    TVar{} -> prettyType t
    TUnit -> prettyType t
    TUserDefined{} -> prettyType t
    TList{} -> prettyType t
    TRecord{} -> prettyType t
    _ -> parens (prettyType t)

prettyTypeAppFun :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => Type loc p -> Doc AnsiStyle
prettyTypeAppFun t@(Type _ _ node) = case node of
    TVar{} -> prettyType t
    TUnit -> prettyType t
    TUserDefined{} -> prettyType t
    TList{} -> prettyType t
    TRecord{} -> prettyType t
    TApp{} -> prettyType t
    _ -> parens (prettyType t)

-- | Pretty-print a declaration body
prettyDeclarationBody :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => DeclarationBody' loc p -> Doc AnsiStyle
prettyDeclarationBody = \case
    ValueDeclaration name expr pats _mTy _meta _anns ->
        group
            ( "let"
                <+> prettyTopValueBinder @p @loc name
                <> prettyValueDeclPatterns @p @loc pats
                <+> "="
                <> nest indentDepth (flatAlt (line <> prettyExpr expr) (" " <> prettyExpr expr))
            )
            <> line
    TypeDeclarationBody name vars typeDecl _mKind _meta _anns ->
        prettyTypeDeclBody name vars typeDecl <> line
    DeclBodyExtension ext -> prettyDeclBodyExtension @p @loc ext

prettyTypeDeclBody :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => TopTypeBinder p loc -> [TypeVariable p loc] -> TypeDeclaration loc p -> Doc AnsiStyle
prettyTypeDeclBody name vars = \case
    Alias t -> header <+> "=" <+> prettyType t
    ADT ctors ->
        let ctorDocs = map prettyCtor (toList ctors)
         in case ctorDocs of
                [] -> header <+> "="
                _ -> header <+> "=" <+> prettyCtorsInline ctorDocs
  where
    header =
        "type" <+> prettyTopTypeBinder @p @loc name <> case vars of
            [] -> mempty
            _ -> " " <> hsep (map (prettyTypeVariable @p @loc) vars)
    prettyCtor (cname, args) =
        prettyConstructorBinder @p @loc cname <> case args of
            [] -> mempty
            _ -> " " <> hsep (map prettyTypeAtom args)

-- | Pretty-print a declaration
prettyDeclaration :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => Declaration loc p -> Doc AnsiStyle
prettyDeclaration (Declaration _ (Declaration' _ (DeclarationBody _ body))) =
    prettyDeclarationBody @loc @p body

-- | Pretty-print a module with all its declarations
prettyModule :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc, Pretty (Locate loc ModuleName)) => Module loc p -> Doc AnsiStyle
prettyModule (Module _ m) =
    "module"
        <+> pretty (moduleName m)
        <> line
        <> line
        <> vsep (map prettyDeclaration (moduleDeclarations m))

-- | Pretty-print a typed lambda parameter (used by Renamed, Shunted, Typed)
prettyTypedLambdaParam :: forall loc v p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc, Pretty v) => TypedLambdaParam v loc p -> Doc AnsiStyle
prettyTypedLambdaParam (TypedLambdaParam v meta) =
    case prettyPatternMeta @p @loc meta of
        Nothing -> pretty v
        Just t -> parens (pretty v <+> ":" <+> t)

-- | Pretty-print a binary operator expression extension
prettyBinaryOperatorExt :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => BinaryOperatorExtension loc p -> Doc AnsiStyle
prettyBinaryOperatorExt (BinaryOperatorExpression op lhs rhs) =
    prettyExprAtom lhs <+> prettyBinaryOperator op <+> prettyExpr rhs

-- | Pretty-print a parenthesized expression extension
prettyInParensExt :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => InParensExtension loc p -> Doc AnsiStyle
prettyInParensExt (InParensExpression e) = prettyExpr e

-- | Pretty-print a list expression extension
prettyListExprExt :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => ListExprExtension loc p -> Doc AnsiStyle
prettyListExprExt (ListExpression es) = "[" <+> hsep (punctuate "," (map prettyExpr es)) <+> "]"

-- | Pretty-print a tuple expression extension
prettyTupleExprExt :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => TupleExprExtension loc p -> Doc AnsiStyle
prettyTupleExprExt (TupleExpression items) = parens (hsep (punctuate "," (map prettyExpr (AtLeast2List.toList items))))

-- | Pretty-print a list/tuple/cons pattern extension
prettyListTuplePatternExt :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => ListTuplePatternExtension loc p -> Doc AnsiStyle
prettyListTuplePatternExt = \case
    ListPattern ps -> "[" <+> hsep (punctuate "," (map prettyPattern ps)) <+> "]"
    TuplePattern ps -> parens (hsep (punctuate "," (map prettyPattern (toList ps))))
    ConsPattern hd tl -> parens (prettyPattern hd <+> "::" <+> prettyPattern tl)

-- | Pretty-print a tuple type extension
prettyTupleTypeExt :: forall loc p. (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => TupleTypeExtension loc p -> Doc AnsiStyle
prettyTupleTypeExt (TupleType items) = parens (hsep (punctuate "," (map prettyType (AtLeast2List.toList items))))
