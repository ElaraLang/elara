{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Elara.AST.Generic.Instances.Pretty where

import Data.Generics.Wrapped
import Elara.AST.Generic.Types
import Elara.AST.Generic.Utils
import Elara.AST.Name
import Elara.AST.Pretty
import Elara.AST.Select
import Elara.AST.StripLocation
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles
import Prelude hiding (group)

deriving instance Pretty (ASTLocate ast (BinaryOperator' ast)) => Pretty (BinaryOperator ast)

instance
    ( Pretty (CleanupLocated (ASTLocate' ast (Select SymOp ast)))
    , (Pretty (Select Infixed ast))
    ) =>
    Pretty (BinaryOperator' ast)
    where
    pretty (SymOp op) = pretty op
    pretty (Infixed op) = "`" <> pretty op <> "`"

instance
    ( Pretty (ASTLocate ast (Type' ast))
    , ToMaybe (Select TypeKind ast) (Maybe typeKind)
    , typeKind ~ UnwrapMaybe (Select TypeKind ast)
    , Pretty typeKind
    ) =>
    Pretty (Type ast)
    where
    pretty (Type (t, k)) = pretty t <+> pretty kAnn
      where
        kAnn = (":" <+>) . pretty <$> (toMaybe k :: Maybe typeKind)

instance
    Pretty (ASTLocate ast (Declaration' ast)) =>
    Pretty (Declaration ast)
    where
    pretty (Declaration ldb) = pretty ldb

data UnknownPretty = forall a. Pretty a => UnknownPretty a

instance Pretty UnknownPretty where
    pretty (UnknownPretty a) = pretty a

instance
    ( Pretty (Expr ast)
    , Pretty (ASTLocate ast (Select ASTTypeVar ast))
    , Pretty (Select (Annotations ForValueDecl) ast)
    , Pretty (ASTLocate ast (Select (ASTName ForValueDecl) ast))
    , Pretty (ASTLocate ast (Select (ASTName ForType) ast))
    , Pretty (ASTLocate ast (TypeDeclaration ast))
    , Pretty (Select ValueTypeDef ast)
    , Pretty valueType
    , ToMaybe (Select (ASTType ForValueDecl) ast) (Maybe valueType)
    , valueType ~ UnwrapMaybe (Select (ASTType ForValueDecl) ast)
    , Pretty exprType
    , exprType ~ UnwrapMaybe (Select (ASTType ForExpr) ast)
    , ToMaybe (Select (ASTType ForExpr) ast) (Maybe exprType)
    , RUnlocate (ast :: b)
    ) =>
    Pretty (Declaration' ast)
    where
    pretty (Declaration' _ b) =
        let val = b ^. _Unwrapped
            y = rUnlocate @b @ast @(DeclarationBody' ast) val
         in prettyDB y
      where
        -- The type of a 'Value' can appear in 2 places: Either as a field in 'Value''s constructor, or as the second field of the 'Expr' tuple
        -- We know that only one will ever exist at a time (in theory, this isn't a formal invariant) so need to find a way of handling both cases
        -- The fields have different types, but both are required to have a Pretty instance (see constraints above).
        -- 'prettyValueDeclaration' takes a 'Pretty a3 => Maybe a3' as its third argument, representing the type of the value.
        -- To make the two compatible, we create an existential wrapper 'UnknownPretty' which has a 'Pretty' instance, and use that as the type of the third argument.
        -- The converting of values to a 'Maybe' is handled by the 'ToMaybe' class.
        prettyDB (Value n e@(Expr (_, t)) _ t' ann) =
            let typeOfE =
                    -- Prioritise the type in the declaration (as this is either a type the user explicitly wrote, or a generalised version)
                    ( UnknownPretty
                        <$> (toMaybe t' :: Maybe valueType)
                    )
                        <|> ( UnknownPretty
                                <$> (toMaybe t :: Maybe exprType) -- If it's not in the declaration, use the type of the expression
                            )
             in prettyValueDeclaration n e typeOfE ann
        prettyDB (TypeDeclaration n vars t ann) = prettyTypeDeclaration n vars t ann
        prettyDB (ValueTypeDef n t ann) = prettyValueTypeDef n t (Just ann)

instance
    ( Pretty (Select Alias ast)
    , Pretty (Select ADTParam ast)
    , Pretty (ASTLocate ast (Select ConstructorName ast))
    ) =>
    Pretty (TypeDeclaration ast)
    where
    pretty = \case
        ADT c -> prettyADT c
          where
            prettyADT :: NonEmpty (CleanupLocated (ASTLocate' ast (Select ConstructorName ast)), [Select ADTParam ast]) -> Doc AnsiStyle
            prettyADT = hsep . punctuate "|" . map (\(name, params) -> pretty name <+> hsep (pretty <$> params)) . toList
        Alias a -> pretty a

instance
    ( exprType ~ UnwrapMaybe (Select (ASTType ForExpr) ast) -- This constraint fixes ambiguity errors
    , letPatterns ~ UnwrapList (Select LetPattern ast)
    , lambdaPatterns ~ UnwrapList (Select LambdaPattern ast)
    , Pretty (ASTLocate ast (Select ConRef ast))
    , Pretty (ASTLocate ast (Select ASTVarRef ast))
    , Pretty (Select TypeApplication ast)
    , (Pretty (ASTLocate ast (Select LetParamName ast)))
    , Pretty letPatterns
    , letPatterns ~ UnwrapList (Select LetPattern ast)
    , (ToList (Select LetPattern ast) [letPatterns])
    , (ToList (Select List ast) [Expr ast])
    , Pretty lambdaPatterns
    , lambdaPatterns ~ UnwrapList (Select LambdaPattern ast)
    , (ToList (ASTLocate ast (Select LambdaPattern ast)) [lambdaPatterns])
    , (Pretty (ASTLocate ast (BinaryOperator' ast)))
    , (ToMaybe (Select (ASTType ForExpr) ast) (Maybe (UnwrapMaybe (Select (ASTType ForExpr) ast))))
    , (ToMaybe (Select PatternType ast) (Maybe (UnwrapMaybe (Select PatternType ast))))
    , (Pretty (UnwrapMaybe (Select (ASTType ForExpr) ast)))
    , (Pretty (UnwrapMaybe (Select PatternType ast)))
    , (Pretty (CleanupLocated (ASTLocate' ast (Pattern' ast))))
    , (StripLocation (ASTLocate ast (Expr' ast)) (Expr' ast))
    , (Pretty (Select (ASTType ForExpr) ast))
    , (DataConAs (Select ASTBinaryOperator ast) (BinaryOperator ast, Expr ast, Expr ast))
    , (DataConAs (Select InParens ast) (Expr ast))
    , DataConAs (Select Tuple ast) (NonEmpty (Expr ast))
    , RUnlocate ast
    ) =>
    Pretty (Expr ast)
    where
    pretty =
        let ?withType = True
            ?contextFree = True
         in prettyExpr @ast @exprType @letPatterns @lambdaPatterns

prettyExpr ::
    forall ast exprType letPatterns lambdaPatterns.
    ( exprType ~ UnwrapMaybe (Select (ASTType ForExpr) ast) -- This constraint fixes ambiguity errors
    , letPatterns ~ UnwrapList (Select LetPattern ast)
    , lambdaPatterns ~ UnwrapList (Select LambdaPattern ast)
    , (?contextFree :: Bool, ?withType :: Bool)
    , _
    ) =>
    Expr ast ->
    Doc AnsiStyle
prettyExpr (Expr (e, t)) = group (flatAlt long short)
  where
    te = if ?withType then (":" <+>) . pf . pretty <$> (toMaybe t :: Maybe exprType) else Nothing
    pe = prettyExpr' (stripLocation @(ASTLocate ast (Expr' ast)) @(Expr' ast) e)
    pf = if ?contextFree then parens else identity
    long = pe <> pretty te
    short = align (pretty pe <> pretty te)

instance
    forall ast letPatterns lambdaPatterns.
    ( Pretty (ASTLocate ast (Select ConRef ast))
    , Pretty (ASTLocate ast (Select ASTVarRef ast))
    , Pretty (Select TypeApplication ast)
    , Pretty (ASTLocate ast (Select LetParamName ast))
    , Pretty letPatterns
    , letPatterns ~ UnwrapList (Select LetPattern ast)
    , ToList (Select LetPattern ast) [letPatterns]
    , ToList (Select List ast) [Expr ast]
    , Pretty lambdaPatterns
    , lambdaPatterns ~ UnwrapList (Select LambdaPattern ast)
    , (ToList (ASTLocate ast (Select LambdaPattern ast)) [lambdaPatterns])
    , (Pretty (ASTLocate ast (BinaryOperator' ast)))
    , (ToMaybe (Select (ASTType ForExpr) ast) (Maybe (UnwrapMaybe (Select (ASTType ForExpr) ast))))
    , (ToMaybe (Select PatternType ast) (Maybe (UnwrapMaybe (Select PatternType ast))))
    , (Pretty (UnwrapMaybe (Select (ASTType ForExpr) ast)))
    , (Pretty (UnwrapMaybe (Select PatternType ast)))
    , (Pretty (CleanupLocated (ASTLocate' ast (Pattern' ast))))
    , (StripLocation (CleanupLocated (ASTLocate' ast (Expr' ast))) (Expr' ast))
    , (Pretty (Select (ASTType ForExpr) ast))
    , (DataConAs (Select ASTBinaryOperator ast) (BinaryOperator ast, Expr ast, Expr ast))
    , (DataConAs (Select InParens ast) (Expr ast))
    , DataConAs (Select Tuple ast) (NonEmpty (Expr ast))
    , RUnlocate ast
    ) =>
    Pretty (Expr' ast)
    where
    pretty e =
        let ?contextFree = True
            ?withType = True
         in prettyExpr' @ast @letPatterns @lambdaPatterns e

prettyExpr' ::
    forall ast letPatterns lambdaPatterns.
    ( lambdaPatterns ~ UnwrapList (Select LambdaPattern ast)
    , letPatterns ~ UnwrapList (Select LetPattern ast)
    , ?contextFree :: Bool
    , ?withType :: Bool
    , _
    ) =>
    Expr' ast ->
    Doc AnsiStyle
prettyExpr' (Int i) = scalar (pretty i)
prettyExpr' (Float f) = scalar (pretty f)
prettyExpr' (String s) = scalar (pretty '\"' <> pretty s <> pretty '\"')
prettyExpr' (Char c) = scalar ("'" <> escapeChar c <> "'")
prettyExpr' Unit = scalar "()"
prettyExpr' (Var v) = varName (pretty v)
prettyExpr' (Constructor c) = typeName (pretty c)
prettyExpr' (Lambda ps e) =
    prettyLambdaExpr
        (fieldToList @(ASTLocate ast (Select LambdaPattern ast)) ps :: [lambdaPatterns])
        (prettyExpr e)
prettyExpr' (FunctionCall e1 e2) = prettyFunctionCallExpr e1 e2 False
prettyExpr' (TypeApplication e1 e2) = prettyFunctionCall e1 ("@" <> parens (pretty e2))
prettyExpr' (If e1 e2 e3) = prettyIfExpr (prettyExpr e1) (prettyExpr e2) (prettyExpr e3)
prettyExpr' (List l) =
    prettyList (prettyExpr <$> fieldToList @(Select List ast) @[Expr ast] l)
prettyExpr' (Match e m) = prettyMatchExpr (prettyExpr e) (prettyMatchBranch . second prettyExpr <$> m)
prettyExpr' (LetIn v p e1 e2) = prettyLetInExpr v (fieldToList @(Select LetPattern ast) p :: [letPatterns]) e1 e2
prettyExpr' (Let v p e) = prettyLetExpr v (fieldToList @(Select LetPattern ast) p :: [letPatterns]) e
prettyExpr' (Block b) = prettyBlockExpr (prettyExpr <$> b)
prettyExpr' (Tuple t) =
    let t' = dataConAs @(Select Tuple ast) @(NonEmpty (Expr ast)) t
     in prettyTupleExpr (prettyExpr <$> t')
prettyExpr' (BinaryOperator b) =
    let (op, e1, e2) = dataConAs @(Select ASTBinaryOperator ast) @(BinaryOperator ast, Expr ast, Expr ast) b
     in prettyBinaryOperatorExpr e1 op e2
prettyExpr' (InParens e) =
    let e' = dataConAs @(Select InParens ast) @(Expr ast) e
     in parens (prettyExpr e')

instance
    ( Pretty a1
    , ToMaybe (Select PatternType ast) (Maybe a1)
    , a1 ~ UnwrapMaybe (Select PatternType ast)
    , (Pretty (CleanupLocated (ASTLocate' ast (Pattern' ast))))
    ) =>
    Pretty (Pattern ast)
    where
    pretty (Pattern (p, t)) = group (flatAlt long short)
      where
        te = (":" <+>) . pretty <$> (toMaybe t :: Maybe a1)
        long = pretty p <+> pretty te
        short = align (pretty p <+> pretty te)

instance
    ( Pretty (CleanupLocated (ASTLocate' ast (Select VarPat ast)))
    , Pretty (CleanupLocated (ASTLocate' ast (Select ConPat ast)))
    , (ToMaybe (Select PatternType ast) (Maybe (UnwrapMaybe (Select PatternType ast))))
    , (Pretty (UnwrapMaybe (Select PatternType ast)))
    , (Pretty (CleanupLocated (ASTLocate' ast (Pattern' ast))))
    , DataConAs (Select ConsPattern ast) (Pattern ast, Pattern ast)
    , DataConAs (Select ListPattern ast) [Pattern ast]
    , DataConAs (Select TuplePattern ast) (NonEmpty (Pattern ast))
    ) =>
    Pretty (Pattern' ast)
    where
    pretty = let ?contextFree = True in prettyPattern

prettyPattern ::
    forall ast.
    (?contextFree :: Bool, _) =>
    Pattern' ast ->
    Doc AnsiStyle
prettyPattern (VarPattern v) = pretty v
prettyPattern (ConstructorPattern c ps) = prettyConstructorPattern c ps
prettyPattern (ListPattern l) = prettyList (dataConAs @(Select ListPattern ast) @[Pattern ast] l)
prettyPattern (TuplePattern t) = prettyTupleExpr (pretty <$> dataConAs @(Select TuplePattern ast) @(NonEmpty (Pattern ast)) t)
prettyPattern (ConsPattern c) = do
    let (p1, p2) = dataConAs @(Select ConsPattern ast) @(Pattern ast, Pattern ast) c
    prettyConsPattern p1 p2
prettyPattern WildcardPattern = "_"
prettyPattern (IntegerPattern i) = pretty i
prettyPattern (FloatPattern f) = pretty f
prettyPattern (StringPattern s) = pretty '\"' <> pretty s <> pretty '\"'
prettyPattern (CharPattern c) = "'" <> escapeChar c <> "'"
prettyPattern UnitPattern = "()"

instance
    ( Pretty (ASTLocate ast (Type' ast))
    , Pretty (ASTLocate ast LowerAlphaName)
    , Pretty (ASTLocate ast (Select ASTTypeVar ast))
    , Pretty (ASTLocate ast (Select UserDefinedType ast))
    , ToMaybe (Select TypeKind ast) (Maybe typeKind)
    , typeKind ~ UnwrapMaybe (Select TypeKind ast)
    , Pretty typeKind
    ) =>
    Pretty (Type' ast)
    where
    pretty = \case
        TypeVar name -> pretty name
        FunctionType a b -> parens (pretty a <+> "->" <+> pretty b)
        UnitType -> "()"
        TypeConstructorApplication a b -> pretty a <+> pretty b
        UserDefinedType name -> pretty name
        RecordType fields -> "{" <+> prettyFields fields <+> "}"
        TupleType fields -> tupled (map pretty (toList fields))
        ListType a -> "[" <+> pretty a <+> "]"
      where
        prettyFields = hsep . punctuate "," . map (\(name, value) -> pretty name <+> ":" <+> pretty value) . toList

instance
    ( Pretty v
    , ToMaybe (Select PatternType ast) (Maybe pt)
    , pt ~ UnwrapMaybe (Select PatternType ast)
    , Pretty pt
    ) =>
    Pretty (TypedLambdaParam v ast)
    where
    pretty (TypedLambdaParam (v, t)) = case toMaybe t of
        Just t' -> pretty v <+> ":" <+> pretty @pt t'
        Nothing -> pretty v

deriving instance Pretty (Select (Annotations ForValueDecl) ast) => Pretty (ValueDeclAnnotations ast)

deriving instance Pretty (Expr ast) => Pretty (AnnotationArg ast)
instance (Pretty (ASTLocate ast (Select AnnotationName ast)), Pretty (AnnotationArg ast)) => Pretty (Annotation ast) where
    pretty (Annotation name args) = punctuation "#" <> pretty name <> hsep (pretty <$> args)
