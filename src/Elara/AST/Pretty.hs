-- | Common module for all the different AST pretty impls
module Elara.AST.Pretty where

import Elara.Data.Pretty
import Prelude hiding (group)

none :: [()]
none = []

prettyStringExpr :: Text -> Doc AnsiStyle
prettyStringExpr = dquotes . pretty

prettyCharExpr :: Char -> Doc AnsiStyle
prettyCharExpr = squotes . escapeChar

prettyLambdaExpr :: (Pretty a, Pretty b) => [a] -> b -> Doc AnsiStyle
prettyLambdaExpr args body = parens ("\\" <> hsep (pretty <$> args) <+> "->" <+> pretty body)

prettyFunctionCallExpr :: (Pretty a, Pretty b) => a -> b -> Doc AnsiStyle
prettyFunctionCallExpr e1 e2 = parens (pretty e1 <+> pretty e2)

prettyIfExpr :: (Pretty a, Pretty b, Pretty c) => a -> b -> c -> Doc AnsiStyle
prettyIfExpr e1 e2 e3 = parens ("if" <+> pretty e1 <+> "then" <+> pretty e2 <+> "else" <+> pretty e3)

prettyBinaryOperatorExpr :: (Pretty a, Pretty b, Pretty c) => a -> b -> c -> Doc AnsiStyle
prettyBinaryOperatorExpr e1 o e2 = parens (pretty e1 <+> pretty o <+> pretty e2)

prettyListExpr :: (Pretty a) => [a] -> Doc AnsiStyle
prettyListExpr l = list (pretty <$> l)

prettyMatchExpr :: (Pretty a, Pretty b) => a -> b -> Doc AnsiStyle
prettyMatchExpr e m = parens ("match" <+> pretty e <+> "with" <+> pretty m)

prettyMatchBranch :: (Pretty a1, Pretty a2) => (a1, a2) -> Doc AnsiStyle
prettyMatchBranch (p, e) = pretty p <+> "->" <+> pretty e

prettyLetInExpr :: (Pretty a1, Pretty a2, Pretty a3, Pretty a4) => a1 -> [a2] -> a3 -> a4 -> Doc AnsiStyle
prettyLetInExpr v ps e1 e2 = parens ("let" <+> pretty v <+> hsep (pretty <$> ps) <+> "=" <+> pretty e1 <+> "in" <+> pretty e2)

prettyLetExpr :: (Pretty a1, Pretty a2, Pretty a3) => a1 -> [a2] -> a3 -> Doc AnsiStyle
prettyLetExpr v ps e = "let" <+> pretty v <+> hsep (pretty <$> ps) <+> "=" <+> pretty e

prettyBlockExpr :: (Pretty a, Foldable t) => t a -> Doc AnsiStyle
prettyBlockExpr b = do
  let open = flatAlt "" "{ "
      close = flatAlt "" " }"
      separator = flatAlt "" "; "
  group (align (encloseSep open close separator (pretty <$> toList b)))

prettyConstructorPattern :: (Pretty a1, Pretty a2) => a1 -> [a2] -> Doc AnsiStyle
prettyConstructorPattern c p = parens (pretty c <+> hsep (pretty <$> p))

prettyListPattern :: (Pretty a) => [a] -> Doc AnsiStyle
prettyListPattern l = list (pretty <$> l)

prettyConsPattern :: (Pretty a1, Pretty a2) => a1 -> a2 -> Doc AnsiStyle
prettyConsPattern e1 e2 = parens (pretty e1 <+> "::" <+> pretty e2)