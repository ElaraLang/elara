module Elara.AST.Frontend.Pretty.Unlocated (prettyPrint) where

import Data.Text (length)
import Elara.AST.Frontend.Unlocated
import Elara.AST.Name (MaybeQualified (..), ModuleName (ModuleName), NameLike (fullNameText), OpName (OpName), TypeName (TypeName), VarName (..))
import Text.PrettyPrint
import Text.PrettyPrint qualified as PP
import Prelude hiding (Op, length, (<>))

indentDepth :: Int
indentDepth = 2

parensIf :: Bool -> Doc -> Doc
parensIf True = PP.parens
parensIf False = id

prettyPrint :: (Pretty p) => p -> Text
prettyPrint = toText . render . ppr 0

class Pretty p where
    ppr :: Int -> p -> Doc

instance Pretty Expr where
    ppr _ (Int i) = PP.integer i
    ppr _ (Float f) = PP.double f
    ppr _ (String s) = PP.char '"' <> PP.text (toString s) <> PP.char '"'
    ppr _ (Char c) = PP.char '\'' <> PP.char c <> PP.char '\''
    ppr _ Unit = "()"
    ppr p (Var v) = ppr p v
    ppr p (Constructor c) = ppr p c
    ppr p (Lambda ps e) = parensIf (p > 0) $ char '\\' <> hsep (fmap (ppr 0) ps) <+> "->" <+> ppr (p + 1) e
    ppr p (FunctionCall e1 e2) = PP.parens (PP.parens (ppr p e1) <+> PP.parens (ppr (p + 1) e2)) -- until operator precedence is implemented, this is the only way to get the correct precedence
    ppr p (If e1 e2 e3) =
        parensIf (p > 0) $
            vcat
                [ "if" $+$ nest indentDepth (ppr 0 e1)
                , "then" $+$ nest indentDepth (ppr 0 e2)
                , "else" $+$ nest indentDepth (ppr 0 e3)
                ]
    ppr p (BinaryOperator op e1 e2) = PP.parens (ppr p e1 <+> ppr p op <+> ppr p e2)
    ppr p (List es) = PP.brackets (PP.hsep (PP.punctuate ", " (fmap (ppr p) es)))
    ppr p (LetIn n patterns val body) =
        hang ("let" <+> ppr p n <+> hsep (fmap (ppr p) patterns)) (indentDepth + 2 + length (fullNameText n)) ("=" <+> ppr p val)
            $$ nest 1 "in"
            $+$ nest indentDepth (ppr p body)
    ppr p (Let n patterns body) =
        hang ("let" <+> ppr p n <+> hsep (fmap (ppr p) patterns) <+> "=") (indentDepth + 2 + length (fullNameText n)) (ppr p body)
    ppr p (Block es) = PP.vcat $ toList (fmap (ppr p) es)
    ppr p (InParens e) = PP.parens (ppr p e)

instance {-# OVERLAPPABLE #-} Pretty x => Pretty (MaybeQualified x) where
    ppr p (MaybeQualified n (Just m)) = ppr p m <> "." <> ppr p n
    ppr p (MaybeQualified n Nothing) = ppr p n

instance Pretty ModuleName where
    ppr _ (ModuleName m) = PP.hcat (PP.punctuate "." (fmap (PP.text . toString) (toList m)))

instance Pretty VarName where
    ppr _ (NormalVarName n) = PP.text (toString n)
    ppr p (OperatorVarName n) = "(" <> ppr p n <> ")"

instance Pretty (MaybeQualified VarName) where
    ppr p (MaybeQualified (OperatorVarName n) (Just q)) = "(" <> ppr p q <> "." <> ppr p n <> ")"
    ppr p (MaybeQualified n (Just q)) = ppr p q <> "." <> ppr p n
    ppr p (MaybeQualified n Nothing) = ppr p n

instance Pretty TypeName where
    ppr _ (TypeName n) = PP.text (toString n)

instance Pretty OpName where
    ppr _ (OpName n) = PP.text (toString n)

instance Pretty Pattern where
    ppr _ (NamedPattern n) = PP.text (toString n)
    ppr p (ConstructorPattern c ps) = PP.parens (ppr p c <+> PP.hsep (fmap (ppr p) ps))
    ppr p (ListPattern ps) = PP.brackets (PP.hsep (PP.punctuate ", " (fmap (ppr p) ps)))
    ppr _ WildcardPattern = "_"

instance Pretty BinaryOperator where
    ppr _ (Op o) = ppr 0 o
    ppr _ (Infixed i) = "`" <> ppr 0 i <> "`"
