module Elara.Core where

import Elara.AST.Name (Qualified)
import Elara.AST.Pretty (none, prettyBlockExpr, prettyFunctionCallExpr, prettyLambdaExpr, prettyLetInExpr, prettyMatchBranch, prettyStringExpr)
import Elara.AST.VarRef (UnlocatedVarRef)
import Elara.Data.Kind (ElaraKind)
import Elara.Data.Pretty (AnsiStyle, Doc, Pretty (pretty), align, hardline, indentDepth, nest, parens, vsep, (<+>))
import Elara.Data.Unique (Unique)
import Prelude hiding (Alt)

data TypeVariable = TypeVariable
    { tvName :: Unique Text
    , tvKind :: ElaraKind
    }
    deriving (Show)

data Var
    = TyVar TypeVariable
    | Id
        { idVarName :: UnlocatedVarRef Text
        , idVarType :: Type
        }
    deriving (Show)

data Expr b
    = Var Var
    | Lit Literal
    | App (Expr b) (Expr b)
    | Lam b (Expr b)
    | Let (Bind b) (Expr b)
    | Match (Expr b) (Maybe b) [Alt b]
    | Type Type
    deriving (Show)

type CoreExpr = Expr Var

type CoreAlt = Alt Var

data Bind b
    = Recursive [(b, Expr b)]
    | NonRecursive (b, Expr b)
    deriving (Show)

type Alt b = (AltCon, [b], Expr b)

data AltCon
    = DataAlt DataCon
    | LitAlt Literal
    | DEFAULT
    deriving (Show)

-- | A data constructor.
data DataCon = DataCon
    { name :: Qualified Text
    , dataConType :: Type
    }
    deriving (Show)

data Type
    = TyVarTy TypeVariable
    | FuncTy Type Type
    | AppTy Type Type
    | -- | A type constructor
      ConTy (Qualified Text)
    | ForAllTy TypeVariable Type
    deriving (Show)

data Literal
    = Int Integer
    | String Text
    | Char Char
    | Double Double
    | Unit
    deriving (Show)

instance Pretty CoreExpr where
    pretty = \case
        Var v -> prettyVar False v
        Lit l -> pretty l
        App e1 e2 -> parens (prettyFunctionCallExpr e1 e2)
        Lam b e -> prettyLambdaExpr [prettyVar True b] e
        Let (Recursive binds) e ->
            "Rec"
                <+> prettyBlockExpr
                    ( fmap
                        ( \(bindName, bindVal) -> prettyLetInExpr (prettyVar True bindName) none bindVal e
                        )
                        binds
                    )
        Let (NonRecursive (b, e)) e' -> prettyLetInExpr (prettyVar True b) none e e'
        Match e b alts ->
            let prettyAlts = fmap (\(con, _, b') -> prettyMatchBranch (con, b')) alts
             in "match"
                    <+> pretty e
                    <+> "as"
                    <+> pretty (prettyVar False <$> b)
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
        AppTy t1 t2 -> parens (pretty t1 <+> pretty t2)
        ConTy name -> pretty name
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

prettyVar :: Bool -> Var -> Doc AnsiStyle
prettyVar withType = \case
    TyVar tv -> prettyTypeVariable withType tv
    Id name t -> if withType then parens (pretty name <+> ":" <+> pretty t) else pretty name

prettyTypeVariable :: Bool -> TypeVariable -> Doc AnsiStyle
prettyTypeVariable withKind = \case
    TypeVariable name kind -> if withKind then parens (pretty name <+> ":" <+> pretty kind) else pretty name
