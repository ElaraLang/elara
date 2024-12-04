{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Elara.Core.Pretty where

import Elara.AST.Name (unqualified)
import Elara.AST.Pretty
import Elara.AST.VarRef
import Elara.Core
import Elara.Core.ANF qualified as ANF
import Elara.Core.Generic (Bind (..))
import Elara.Core.ToANF (fromANF, fromANFAtom, fromANFCExpr)
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles
import Prettyprinter.Render.Terminal qualified as Style
import Prelude hiding (Alt, group)

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
        Id name t _ ->
            let style = case name of (Global _) -> Style.color Style.Green; (Local _) -> Style.color Style.Blue
             in if withType
                    then
                        (if withParens then parens else identity)
                            (annotate style (pretty name) <+> punctuation ":" <+> pretty t)
                    else annotate style $ pretty name

    prettyVarArg = \case
        TyVar (TypeVariable tv _) -> parens (punctuation "@" <> pretty tv)
        v -> prettyVar True True v

instance PrettyVar Type where
    prettyVar withType withParens = \case
        TyVarTy tv -> prettyTypeVariable withType tv
        ConTy (TyCon name details) -> pretty name
        other -> prettyVar withType withParens other

    prettyVarArg = \case
        TyVarTy (TypeVariable tv _) -> parens (punctuation "@" <> pretty tv)
        v -> prettyVar True True v

instance {-# OVERLAPS #-} PrettyVar v => Pretty (Expr v) where
    pretty = prettyExpr

prettyTLLam :: (PrettyVar v1, PrettyVar v2) => v1 -> Expr v2 -> Doc AnsiStyle
prettyTLLam b e =
    let (params, body) = collectLamParams b e
     in group $
            vsep
                [ hsep [punctuation "\\", sep params, punctuation "->"]
                , flatAlt (indent indentDepth (prettyExpr body)) (prettyExpr body)
                ]

collectLamParams :: (PrettyVar v1, PrettyVar v2) => v1 -> Expr v2 -> ([Doc AnsiStyle], Expr v2)
collectLamParams first (Lam b e) =
    let (params, body) = collectLamParams b e
     in (prettyVarArg first : params, body)
collectLamParams first e = ([prettyVarArg first], e)

prettyExpr :: (Pretty (Expr v), PrettyVar v, HasCallStack) => Expr v -> Doc AnsiStyle
prettyExpr (Lam b e) = prettyTLLam b e
prettyExpr (TyLam b e) = prettyTLLam b e
prettyExpr (Let bindings e) =
    group $
        vsep
            [ keyword "let" <+> prettyVdefg bindings <+> keyword "in"
            , prettyExpr e
            ]
prettyExpr (Match e of' alts) =
    group $
        vsep
            [ keyword "match" <+> prettyExpr e <+> keyword "with" <+> (maybe "" pretty of') <> lbrace
            , indent indentDepth $ sep (punctuate semi (map prettyAlt alts))
            , rbrace
            ]
prettyExpr other = prettyExpr1 other

prettyExpr1 :: (Pretty (Expr v), PrettyVar v) => Expr v -> Doc AnsiStyle
prettyExpr1 (TyApp f t) = prettyExpr1 f <+> punctuation "@" <> prettyTy2 t
prettyExpr1 (App f x) = prettyExpr1 f <+> prettyExpr2 x
prettyExpr1 e = prettyExpr2 e

prettyExpr2 :: (Pretty (Expr v), PrettyVar v) => Expr v -> Doc AnsiStyle
prettyExpr2 (Var v) = prettyVar False False v
prettyExpr2 (Lit l) = pretty l
prettyExpr2 e = parens (prettyExpr e)

prettyVdefg :: (PrettyVar v, Pretty (expr v)) => Elara.Core.Generic.Bind v expr -> Doc AnsiStyle
prettyVdefg (Recursive bindings) =
    "Rec"
        <+> lbrace
        <+> line
        <> indent indentDepth (vsep (punctuate semi (prettyVdef <$> bindings)))
            <+> line
        <> rbrace
prettyVdefg (NonRecursive b) = prettyVdef b

instance (PrettyVar v, Pretty (e v)) => Pretty (Elara.Core.Generic.Bind v e) where
    pretty = prettyVdefg

prettyVdef :: (PrettyVar v, Pretty (expr v)) => (v, expr v) -> Doc AnsiStyle
prettyVdef (v, e) =
    group $
        vsep
            [ prettyVar True False v <+> "="
            , flatAlt (indent indentDepth (pretty e)) (pretty e)
            ]

prettyVBind :: PrettyVar v => v -> Doc AnsiStyle
prettyVBind = prettyVar False True

prettyAlts :: PrettyVar v => [Alt v] -> Doc AnsiStyle
prettyAlts alts = let ?contextFree = False in prettyBlockExpr (prettyAlt <$> alts)

prettyAlt :: (PrettyVar a, PrettyVar v) => (AltCon, [a], Expr v) -> Doc AnsiStyle
prettyAlt (con, vars, e) =
    pretty @AltCon con
        <> (if null vars then "" else space <> hsep (prettyVarArg <$> vars))
            <+> "->"
        <> line
        <> indent indentDepth (prettyExpr e)

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

prettyTypeVariables :: [TypeVariable] -> Doc AnsiStyle
prettyTypeVariables = \case
    [] -> mempty
    tvs -> hsep (punctuate " " (prettyTypeVariable True <$> tvs))

prettyTy :: Type -> Doc AnsiStyle
prettyTy (FuncTy t1 t2) = prettyTy1 t1 <+> "->" <+> prettyTy t2
prettyTy (ForAllTy tv t) = "∀" <+> prettyTypeVariable False tv <> "." <+> prettyTy t
prettyTy other = prettyTy1 other

prettyTy1 :: Type -> Doc AnsiStyle
prettyTy1 (AppTy t1 t2) = prettyTy1 t1 <+> prettyTy2 t2
prettyTy1 e = prettyTy2 e

prettyTy2 :: Type -> Doc AnsiStyle
prettyTy2 (TyVarTy tv) = prettyTypeVariable False tv
prettyTy2 (ConTy t) = pretty t
prettyTy2 e = parens (prettyTy e)

instance Pretty TyCon where
    pretty (TyCon name details) = pretty (name ^. unqualified)

instance Pretty TyConDetails where
    pretty Prim = "Prim"
    pretty (TyAlias t) = "Alias:" <+> pretty t
    pretty (TyADT ctors) = "ADT:" <+> pretty ctors

instance Pretty AltCon where
    pretty :: AltCon -> Doc AnsiStyle
    pretty = \case
        DataAlt (DataCon name t _) -> pretty name
        LitAlt l -> pretty l
        DEFAULT -> "DEFAULT"

instance Pretty DataCon where
    pretty :: DataCon -> Doc AnsiStyle
    pretty = \case
        DataCon name t _ -> (pretty name <+> ":" <+> pretty t)

prettyTypeVariable :: Bool -> TypeVariable -> Doc AnsiStyle
prettyTypeVariable withKind = \case
    TypeVariable name kind -> if withKind then parens (pretty name <+> ":" <+> pretty kind) else pretty name

-- ANF

instance Pretty (ANF.AExpr Var) where
    pretty = prettyExpr . fromANFAtom

instance Pretty (ANF.CExpr Var) where
    pretty = prettyExpr . fromANFCExpr

instance Pretty (ANF.Expr Var) where
    pretty = prettyExpr . fromANF
