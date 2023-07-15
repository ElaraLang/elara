module Elara.Core where

import Elara.AST.Name (Qualified)
import Elara.AST.Pretty (none, prettyBlockExpr, prettyFunctionCallExpr, prettyLambdaExpr, prettyLetInExpr, prettyStringExpr)
import Elara.AST.VarRef (UnlocatedVarRef)
import Elara.Data.Pretty (AnsiStyle, Doc, Pretty (pretty), hardline, indentDepth, nest, softline, (<+>))
import Prelude hiding (Alt)

type Var = UnlocatedVarRef Text

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

data DataCon = DataCon
  { name :: Qualified Text
  }
  deriving (Show)

data Type
  deriving (Show)

data Literal
  = Int Integer
  | String Text
  | Char Char
  | Double Double
  | Unit
  deriving (Show)

instance Pretty b => Pretty (Expr b) where
  pretty = \case
    Var v -> pretty v
    Lit l -> pretty l
    App e1 e2 -> prettyFunctionCallExpr e1 e2
    Lam b e -> prettyLambdaExpr [b] e
    Let (Recursive binds) e ->
      "Rec"
        <+> prettyBlockExpr
          ( fmap
              ( \(bindName, bindVal) -> prettyLetInExpr bindName none bindVal e
              )
              binds
          )
    Let (NonRecursive (b, e)) e' -> prettyLetInExpr b none e e'
    Match e b alts ->
      "match"
        <+> pretty e
        <+> "as"
        <+> pretty b
        <+> "with"
          <> hardline
          <> nest indentDepth (pretty alts)
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
    _ -> "TODO"

instance Pretty AltCon where
  pretty :: AltCon -> Doc AnsiStyle
  pretty = \case
    DataAlt d -> pretty d
    LitAlt l -> pretty l
    DEFAULT -> "DEFAULT"

instance Pretty DataCon where
  pretty :: DataCon -> Doc AnsiStyle
  pretty = \case
    DataCon name -> pretty name