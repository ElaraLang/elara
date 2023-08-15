{-# OPTIONS_GHC -Wno-orphans #-}

module Elara.Core.Pretty where

import Control.Lens ((^.))
import Elara.AST.Name (unqualified)
import Elara.AST.Pretty
import Elara.Core
import Elara.Data.Pretty
import Prelude hiding (Alt)

class PrettyVar v where
    prettyVar ::
        -- | With type
        Bool ->
        -- | With parens
        Bool ->
        -- | Variable
        v ->
        Doc AnsiStyle

instance PrettyVar Var where
    prettyVar withType withParens = \case
        TyVar tv -> prettyTypeVariable withType tv
        Id name t -> if withType then (if withParens then parens else identity) (pretty name <+> ":" <+> pretty t) else pretty name

instance PrettyVar v => Pretty (Expr v) where
    pretty = prettyExpr

prettyExpr :: (Pretty (Expr v), PrettyVar v) => Expr v -> Doc AnsiStyle
prettyExpr (Lam b e) = prettyLambdaExpr [prettyVar False False b] e
prettyExpr (Let bindings e) = "let" <+> prettyVdefg bindings <+> "in" <+> prettyExpr e
prettyExpr (Match e of' alts) = "case" <+> prettyExpr2 e <+> pretty (("of" <+>) . prettyVBind <$> of') <+> prettyAlts alts
prettyExpr other = prettyExpr1 other

prettyExpr1 :: (Pretty (Expr v), PrettyVar v) => Expr v -> Doc AnsiStyle
prettyExpr1 (App f x) = prettyExpr1 f <+> prettyExpr2 x
prettyExpr1 e = prettyExpr2 e

prettyExpr2 :: (Pretty (Expr v), PrettyVar v) => Expr v -> Doc AnsiStyle
prettyExpr2 (Var v) = prettyVar False False v
prettyExpr2 (Lit l) = pretty l
prettyExpr2 e = parens (prettyExpr e)

prettyVdefg :: (PrettyVar v, Pretty (Expr v)) => Bind v -> Doc AnsiStyle
prettyVdefg (Recursive bindings) = "Rec" <> prettyBlockExpr (prettyVdef <$> bindings)
prettyVdefg (NonRecursive b) = prettyVdef b

prettyVdef :: (PrettyVar v, Pretty (Expr v)) => (v, Expr v) -> Doc AnsiStyle
prettyVdef (v, e) = prettyVar True False v <+> "=" <+> prettyExpr e

prettyVBind :: PrettyVar v => v -> Doc AnsiStyle
prettyVBind = prettyVar True True

prettyAlts :: (Pretty (Expr v), PrettyVar v) => [Alt v] -> Doc AnsiStyle
prettyAlts alts = prettyBlockExpr (prettyAlt <$> alts)
  where
    prettyAlt (con, _, e) = pretty con <+> "->" <+> prettyExpr e

instance Pretty Literal where
    pretty :: Literal -> Doc AnsiStyle
    pretty = \case
        Int i -> pretty i
        String s -> prettyStringExpr s
        Char c -> pretty c
        Double d -> pretty d
        Unit -> "()"

instance Pretty Type where
    pretty :: Type -> Doc AnsiStyle
    pretty = prettyTy

prettyTy :: Type -> Doc AnsiStyle
prettyTy (FuncTy t1 t2) = prettyTy1 t1 <+> "->" <+> prettyTy t2
prettyTy (ForAllTy tv t) = "forall" <+> prettyTypeVariable True tv <> "." <+> prettyTy t
prettyTy other = prettyTy1 other

prettyTy1 :: Type -> Doc AnsiStyle
prettyTy1 (AppTy t1 t2) = prettyTy1 t1 <+> prettyTy2 t2
prettyTy1 e = prettyTy2 e

prettyTy2 :: Type -> Doc AnsiStyle
prettyTy2 (TyVarTy tv) = prettyTypeVariable False tv
prettyTy2 (ConTy name) = pretty (name ^. unqualified)
prettyTy2 e = parens (prettyTy e)

instance Pretty AltCon where
    pretty :: AltCon -> Doc AnsiStyle
    pretty = \case
        DataAlt d -> pretty d
        LitAlt l -> pretty l
        DEFAULT -> "DEFAULT"

instance Pretty DataCon where
    pretty :: DataCon -> Doc AnsiStyle
    pretty = \case
        DataCon name _ -> (pretty name)

prettyTypeVariable :: Bool -> TypeVariable -> Doc AnsiStyle
prettyTypeVariable withKind = \case
    TypeVariable name kind -> if withKind then parens (pretty name <+> ":" <+> pretty kind) else pretty name
