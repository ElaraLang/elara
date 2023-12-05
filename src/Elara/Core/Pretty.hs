{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Elara.Core.Pretty where

import Control.Lens ((^.))
import Elara.AST.Name (unqualified)
import Elara.AST.Pretty
import Elara.Core
import Elara.Data.Pretty
import Elara.Prim.Core (listCon)
import Prelude hiding (Alt)

class Pretty v => PrettyVar v where
    prettyVar ::
        -- | With type
        Bool ->
        -- | With parens
        Bool ->
        -- | Variable
        v ->
        Doc AnsiStyle

    prettyVarArg :: v -> Doc AnsiStyle

instance Pretty Var where
    pretty :: Var -> Doc AnsiStyle
    pretty = prettyVar True True

instance PrettyVar Var where
    prettyVar withType withParens = \case
        TyVar tv -> prettyTypeVariable withType tv
        Id name t -> if withType then (if withParens then parens else identity) (pretty name <+> ":" <+> pretty t) else pretty name

    prettyVarArg = \case
        TyVar (TypeVariable tv _) -> parens ("@" <> pretty tv)
        v -> prettyVar True True v

instance PrettyVar Type where
    prettyVar withType withParens = \case
        TyVarTy tv -> prettyTypeVariable withType tv
        ConTy name -> pretty name
        other -> prettyVar withType withParens other

    prettyVarArg = \case
        TyVarTy (TypeVariable tv _) -> parens ("@" <> pretty tv)
        v -> prettyVar True True v

instance {-# OVERLAPS #-} PrettyVar v => Pretty (Expr v) where
    pretty = prettyExpr

prettyTLLam :: (PrettyVar v1, PrettyVar v2) => v1 -> Expr v2 -> Doc AnsiStyle
prettyTLLam b e@(Lam _ _) = "\\" <+> prettyVarArg b <+> prettyLam e
prettyTLLam b e = "\\" <+> prettyVarArg b <+> "->" <+> prettyExpr e

prettyLam :: (Pretty (Expr v), PrettyVar v) => Expr v -> Doc AnsiStyle
prettyLam (Lam b e@(Lam _ _)) = prettyVarArg b <+> prettyLam e
prettyLam (Lam b e) = prettyVarArg b <+> "->" <+> prettyLam e
prettyLam e = pretty e

prettyExpr :: (Pretty (Expr v), PrettyVar v, HasCallStack) => Expr v -> Doc AnsiStyle
prettyExpr (Lam b e) = prettyTLLam b e
prettyExpr (TyLam b e) = prettyTLLam b e
prettyExpr (Let bindings e) = "let" <+> prettyVdefg bindings <+> "in" <+> prettyExpr e
prettyExpr (Match e of' alts) =
    vsep
        [ "case" <+> prettyExpr e <+> pretty (("of" <+>) . prettyVBind <$> of')
        , indent indentDepth (prettyAlts alts)
        ]
prettyExpr other = prettyExpr1 other

prettyExpr1 :: (Pretty (Expr v), PrettyVar v) => Expr v -> Doc AnsiStyle
prettyExpr1 (TyApp f t) = prettyExpr1 f <+> "@" <> prettyTy2 t
prettyExpr1 (App f x) = prettyExpr1 f <+> prettyExpr2 x
prettyExpr1 e = prettyExpr2 e

prettyExpr2 :: (Pretty (Expr v), PrettyVar v) => Expr v -> Doc AnsiStyle
prettyExpr2 (Var v) = prettyVar False False v
prettyExpr2 (Lit l) = pretty l
prettyExpr2 e = parens (prettyExpr e)

prettyVdefg :: (PrettyVar v, Pretty (Expr v)) => Bind v -> Doc AnsiStyle
prettyVdefg (Recursive bindings) = "Rec" <> let ?contextFree = True in prettyBlockExpr (prettyVdef <$> bindings)
prettyVdefg (NonRecursive b) = prettyVdef b

prettyVdef :: (PrettyVar v, Pretty (Expr v)) => (v, Expr v) -> Doc AnsiStyle
prettyVdef (v, e) = vsep [prettyVar True False v, indent indentDepth ("=" <+> prettyExpr e)]

prettyVBind :: PrettyVar v => v -> Doc AnsiStyle
prettyVBind = prettyVar True True

prettyAlts :: PrettyVar v => [Alt v] -> Doc AnsiStyle
prettyAlts alts = let ?contextFree = True in prettyBlockExpr (prettyAlt <$> alts)
  where
    prettyAlt (con, vars, e) = pretty @AltCon con <+> hsep (prettyVarArg <$> vars) <+> "->" <+> prettyExpr e

instance Pretty Literal where
    pretty :: Literal -> Doc AnsiStyle
    pretty = \case
        Int i -> pretty i
        String s -> prettyStringExpr s
        Char c -> "'" <> pretty c <> "'"
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
prettyTy1 (AppTy l t2) | l == listCon = brackets (prettyTy2 t2)
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
        DataCon name t -> (pretty name <+> ":" <+> pretty t)

prettyTypeVariable :: Bool -> TypeVariable -> Doc AnsiStyle
prettyTypeVariable withKind = \case
    TypeVariable name kind -> if withKind then parens (pretty name <+> ":" <+> pretty kind) else pretty name
