{-# LANGUAGE ImplicitParams #-}

{- | Common module for all the different AST pretty impls.
This includes functions for pretty printing in both context-free and context-sensitive forms
-}
module Elara.AST.Pretty where

import Data.Generics.Wrapped
import Elara.AST.Generic.Types
import Elara.AST.Select (ASTSelector (..), ForSelector (ForValueDecl))
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles
import Prelude hiding (group)

none :: [()]
none = []

{- | A 'Nothing' value for 'Maybe' 'Doc's
Useful for avoiding type annotations
-}
nothing :: Maybe ()
nothing = Nothing

prettyStringExpr :: Text -> Doc AnsiStyle
prettyStringExpr = dquotes . pretty

prettyCharExpr :: Char -> Doc AnsiStyle
prettyCharExpr = squotes . escapeChar

prettyLambdaExpr :: (?contextFree :: Bool, Pretty a, Pretty b) => [a] -> b -> Doc AnsiStyle
prettyLambdaExpr args body = parens (if ?contextFree then prettyCTFLambdaExpr else prettyLambdaExpr')
  where
    prettyCTFLambdaExpr =
        "\\"
            <+> hsep (pretty <$> args)
            <+> "->"
            <+> parens (pretty body)

    prettyLambdaExpr' = group (flatAlt long short)
      where
        short =
            "\\"
                <+> hsep (pretty <$> args)
                <+> "->"
                <+> pretty body

        long =
            align
                ("\\" <+> hsep (pretty <$> args) <+> "->" <> hardline <> nest indentDepth (pretty body))

prettyFunctionCall :: (?contextFree :: Bool, Pretty a, Pretty b) => a -> b -> Doc AnsiStyle
prettyFunctionCall e1' e2' = parens (if ?contextFree then short else group (flatAlt long short))
  where
    short = pretty e1' <+> pretty e2'
    long = pretty e1' <> hardline <> indent indentDepth (pretty e2')

prettyFunctionCallExpr :: (?contextFree :: Bool, RUnlocate ast, Pretty (Expr ast)) => Expr ast -> Expr ast -> Bool -> Doc AnsiStyle
prettyFunctionCallExpr e1 e2 tyApp = prettyFunctionCall e1' e2'
  where
    e1' = parensIf (?contextFree && shouldParen e1) (pretty e1)
    e2' = tyApp' <> parensIf (?contextFree && shouldParen e2) (pretty e2)
    tyApp' = if tyApp then "@" else ""

prettyIfExpr :: (Pretty a, Pretty b, Pretty c) => a -> b -> c -> Doc AnsiStyle
prettyIfExpr e1 e2 e3 = parens (keyword "if" <+> pretty e1 <+> keyword "then" <+> pretty e2 <+> keyword "else" <+> pretty e3)

prettyBinaryOperatorExpr :: (Pretty b, Pretty (Expr ast), RUnlocate ast) => Expr ast -> b -> Expr ast -> Doc AnsiStyle
prettyBinaryOperatorExpr e1 o e2 =
    parens
        ( parensIf (shouldParen e1) (pretty e1)
            <+> pretty o
            <+> parensIf (shouldParen e2) (pretty e2)
        )

prettyTupleExpr :: Pretty a => NonEmpty a -> Doc AnsiStyle
prettyTupleExpr l = parens (hsep (punctuate "," (pretty <$> toList l)))

prettyMatchExpr :: (Pretty a1, Pretty a2, Foldable t, ?contextFree :: Bool) => a1 -> t a2 -> Doc AnsiStyle
prettyMatchExpr e m = parens (keyword "match" <+> pretty e <+> keyword "with" <+> prettyBlockExpr m)

prettyMatchBranch :: (Pretty a1, Pretty a2) => (a1, a2) -> Doc AnsiStyle
prettyMatchBranch (p, e) = pretty p <+> punctuation "->" <+> pretty e

prettyLetInExpr ::
    (Pretty a1, Pretty a2, ?contextFree :: Bool, RUnlocate ast, Pretty (Expr ast)) =>
    a1 ->
    [a2] ->
    Expr ast ->
    Expr ast ->
    Doc AnsiStyle
prettyLetInExpr v ps e1 e2 =
    keyword "let"
        <+> pretty v
        <+> hsep (pretty <$> ps)
        <+> "="
        <+> blockParensIf (?contextFree && shouldBrace e1) (pretty e1)
        <+> keyword "in"
        <+> blockParensIf (?contextFree && shouldBrace e2) (pretty e2)

shouldBrace :: forall astK (ast :: astK). RUnlocate ast => Expr ast -> Bool
shouldBrace x = case (x ^. _Unwrapped % _1 % to (rUnlocate @astK @ast)) :: Expr' ast of
    Block _ -> False
    Let{} -> True
    LetIn{} -> True
    _ -> False

shouldParen :: forall astK (ast :: astK). RUnlocate ast => Expr ast -> Bool
shouldParen x = case (x ^. _Unwrapped % _1 % to (rUnlocate @astK @ast)) :: Expr' ast of
    Block _ -> True
    Let{} -> True
    LetIn{} -> True
    _ -> False

prettyLetExpr :: (Pretty a1, Pretty a2, RUnlocate ast, ?contextFree :: Bool, Pretty (Expr ast)) => a1 -> [a2] -> Expr ast -> Doc AnsiStyle
prettyLetExpr v ps e =
    keyword "let"
        <+> pretty v
        <+> hsep (pretty <$> ps)
        <+> punctuation "="
        <+> blockParensIf (?contextFree && shouldBrace e) (pretty e)

prettyBlockExpr :: (Pretty a, Foldable t, ?contextFree :: Bool) => t a -> Doc AnsiStyle
prettyBlockExpr b = do
    let open = if ?contextFree then "{ " else flatAlt "" "{ "
        close = if ?contextFree then "}" else flatAlt "" " }"
        separator = if ?contextFree then "; " else flatAlt "" "; "
        arrange = if ?contextFree then identity else group . align

    arrange (encloseSep' ?contextFree open close separator (pretty <$> toList b))

encloseSep' :: Bool -> Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle
encloseSep' contextFree = if contextFree then encloseSepUnarranged else encloseSep
  where
    encloseSepUnarranged :: Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle
    encloseSepUnarranged open close _ [] = open <> close
    encloseSepUnarranged open close sep (x : xs) = open <> x <> foldr (\y ys -> sep <> y <> ys) close xs

prettyConstructorPattern :: (Pretty a1, Pretty a2) => a1 -> [a2] -> Doc AnsiStyle
prettyConstructorPattern c p = parens (pretty c <+> hsep (pretty <$> p))

prettyList :: (Pretty a, ?contextFree :: Bool) => [a] -> Doc AnsiStyle
prettyList l =
    if ?contextFree
        then encloseSep' ?contextFree "[ " " ]" ", " (pretty <$> l)
        else list (pretty <$> l)

prettyConsPattern :: (Pretty a1, Pretty a2) => a1 -> a2 -> Doc AnsiStyle
prettyConsPattern e1 e2 = parens (pretty e1 <+> "::" <+> pretty e2)

prettyValueDeclaration ::
    forall ast a1 a2 a3.
    (Pretty a1, Pretty a2, Pretty a3, Pretty (Select (Annotations ForValueDecl) ast)) =>
    a1 -> a2 -> Maybe a3 -> ValueDeclAnnotations ast -> Doc AnsiStyle
prettyValueDeclaration name e expectedType anns =
    let defLine = (\x -> prettyValueTypeDef @ast name x Nothing) <$> expectedType
        annLine = prettyValueDeclAnnotations anns
        rest =
            [ keyword "let" <+> pretty name <+> punctuation "="
            , indent indentDepth (pretty e)
            , "" -- add a newline
            ]
     in vsep (annLine : maybeToList defLine <> rest)

prettyValueDeclAnnotations :: Pretty (Select (Annotations ForValueDecl) ast) => ValueDeclAnnotations ast -> Doc AnsiStyle
prettyValueDeclAnnotations (ValueDeclAnnotations x) = pretty x

prettyValueTypeDef :: forall ast a1 a2. (Pretty a1, Pretty a2, Pretty (Select (Annotations ForValueDecl) ast)) => a1 -> a2 -> Maybe (ValueDeclAnnotations ast) -> Doc AnsiStyle
prettyValueTypeDef name t anns =
    let annLine = fmap ("#" <>) (prettyValueDeclAnnotations <$> anns)
     in vsep [fromMaybe "" annLine, keyword "def" <+> pretty name <+> punctuation ":" <+> pretty t]

prettyTypeDeclaration :: (Pretty a1, Pretty a2, Pretty a3) => a1 -> [a2] -> a3 -> TypeDeclAnnotations ast -> Doc AnsiStyle
prettyTypeDeclaration name vars t _ =
    vsep
        [ keyword "type" <+> typeName (pretty name) <+> hsep (varName . pretty <$> vars)
        , indent indentDepth (punctuation "=" <+> pretty t)
        , "" -- add a newline
        ]
