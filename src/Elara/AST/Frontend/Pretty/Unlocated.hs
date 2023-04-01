module Elara.AST.Frontend.Pretty.Unlocated where

import Control.Lens ((^.))
import Elara.AST.Name (HasName (name), MaybeQualified (..), ModuleName (ModuleName), NameLike (nameText), OpName (OpName), TypeName (TypeName), Unqualified, VarName (..))
import Elara.AST.Unlocated.Frontend
import Elara.Data.Pretty
import Prelude hiding (Op, length, (<>))

-- instance Pretty Expr where
--     pretty (Int i) = pretty i
--     pretty (Float f) = pretty f
--     pretty (String s) = '"' <> fromString (toString s) <> PP.char '"'
--     pretty (Char c) = PP.char '\'' <> escapeChar c <> PP.char '\''
--     pretty Unit = "()"
--     pretty (Var v) = pretty v
--     pretty (Constructor c) = pretty c
--     pretty (Lambda ps e) =
--         parensIf (p > 0) $
--             char '\\' <> hsep (fmap (ppr 0) ps) <+> "->" $+$ nest indentDepth (ppr (p + 1) e)
--     pretty (FunctionCall e1 e2) = parens (parens (pretty e1) <+> parens (ppr (p + 1) e2)) -- until operator precedence is implemented, this is the only way to get the correct precedence
--     pretty (If e1 e2 e3) =
--         parensIf (p > 0) $
--             vcat
--                 [ "if" $+$ nest indentDepth (ppr 0 e1)
--                 , "then" $+$ nest indentDepth (ppr 0 e2)
--                 , "else" $+$ nest indentDepth (ppr 0 e3)
--                 ]
--     pretty (BinaryOperator op e1 e2) = PP.parens (PP.parens (pretty e1) <+> pretty op <+> PP.parens (pretty e2))
--     pretty (List es) = PP.brackets (PP.hsep (PP.punctuate ", " (fmap (pretty) es)))
--     pretty (LetIn n patterns val body) =
--         pretty (Let n patterns val)
--             $+$ nest 1 "in"
--             $+$ nest indentDepth (pretty body)
--     pretty (Let n patterns body) =
--         ("let" <+> pretty n <+> hsep (fmap (pretty) patterns) <+> "=") $+$ nest (indentDepth + 4) (pretty body)
--     pretty (Match e cases) =
--         "match" <+> PP.braces (pretty e) <+> "with" $+$ nest indentDepth (vcat (fmap (pretty) cases))
--     pretty (Block es) = PP.vcat $ toList (fmap (pretty) es)
--     pretty (InParens e) = PP.parens (pretty e)

-- instance Pretty (Pattern, Expr) where
--     pretty (p', e) = pretty p' <+> "->" $+$ nest indentDepth (pretty e)

-- instance Pretty Pattern where
--     pretty (VarPattern n) = PP.text $ toString $ nameText n
--     pretty (ConstructorPattern c ps) = PP.parens (pretty c <+> PP.hsep (fmap (pretty) ps))
--     pretty (ListPattern ps) = PP.brackets (PP.hsep (PP.punctuate ", " (fmap (pretty) ps)))
--     pretty WildcardPattern = "_"
--     pretty (IntegerPattern i) = PP.integer i
--     pretty (FloatPattern f) = PP.double f
--     pretty (StringPattern s) = PP.char '"' <> fromString (toString s) <> PP.char '"'
--     pretty (CharPattern c) = PP.char '\'' <> escapeChar c <> PP.char '\''
-- instance Pretty BinaryOperator where
--     pretty (Op o) = ppr 0 o
--     pretty (Infixed i) = "`" <> ppr 0 i <> "`"
