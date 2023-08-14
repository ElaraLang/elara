{-# OPTIONS_GHC -Wno-orphans #-}

module Elara.Core.Pretty where

import Control.Lens ((^.))
import Elara.AST.Name (Qualified (Qualified), unqualified)
import Elara.AST.Pretty
import Elara.Core
import Elara.Data.Pretty
import Elara.Prim.Core

instance Pretty CoreBind where
    pretty = \case
        Recursive binds ->
            "Rec"
                <+> bracedBlock
                    ( fmap
                        ( \(bindName, bindVal) -> prettyLetInExpr (prettyVar True False bindName) none bindVal nothing
                        )
                        binds
                    )
        NonRecursive (b, e) -> prettyLetInExpr (prettyVar True False b) none e nothing

instance Pretty CoreExpr where
    pretty = \case
        Var v -> prettyVar True True v
        Lit l -> pretty l
        App e1 e2 -> parens (prettyFunctionCallExpr e1 e2)
        Lam b e -> prettyLambdaExpr [prettyVar True True b] e
        Let (Recursive binds) e ->
            "Rec"
                <+> prettyBlockExpr
                    ( fmap
                        ( \(bindName, bindVal) -> prettyLetInExpr (prettyVar True False bindName) none bindVal (Just e)
                        )
                        binds
                    )
        Let (NonRecursive (b, e)) e' -> prettyLetInExpr (prettyVar True False b) none e (Just e')
        Match e b alts ->
            let prettyAlts = fmap (\(con, _, b') -> prettyMatchBranch (con, b')) alts
             in "match"
                    <+> pretty e
                    <+> "as"
                    <+> pretty (prettyVar False False <$> b)
                    <+> "with"
                    <+> hardline
                        <> nest indentDepth (align (vsep prettyAlts))
        Type t -> "@" <> pretty t

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
    pretty = \case
        TyVarTy tv -> prettyTypeVariable False tv
        FuncTy t1 t2 -> parens (pretty t1 <+> "->" <+> pretty t2)
        AppTy c x | c == listCon -> "[" <> pretty x <> "]" -- turn [] x into [x]
        AppTy t1 t2 -> parens (pretty t1 <+> pretty t2)
        ConTy name -> pretty (name ^. unqualified)
        ForAllTy tv t -> parens ("forall" <+> prettyTypeVariable True tv <> "." <+> pretty t)

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

prettyVar :: Bool -> Bool -> Var -> Doc AnsiStyle
prettyVar withType withParens = \case
    TyVar tv -> prettyTypeVariable withType tv
    Id name t -> if withType then (if withParens then parens else identity) (pretty name <+> ":" <+> pretty t) else pretty name

prettyTypeVariable :: Bool -> TypeVariable -> Doc AnsiStyle
prettyTypeVariable withKind = \case
    TypeVariable name kind -> if withKind then parens (pretty name <+> ":" <+> pretty kind) else pretty name
