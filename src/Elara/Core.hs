{- | Elara core language
After type inference, the AST is compiled to this much simpler language,
which makes further stages in the compilation pipeline much easier.
-}
module Elara.Core where

import Elara.AST.Name (Name, Qualified, VarName)
import Elara.Data.Pretty
import Elara.Data.Unique

type CoreExpr = Expr Var

type CoreCase = Case Var

data Var
    = Id (VarRef VarName) Type
    deriving (Show)

data VarRef a
    = Local (Unique a)
    | Global (Qualified a)
    deriving (Show)

-- | Core expression type
data Expr v
    = -- | Variable reference
      Var v
    | -- | Literal value
      Lit Literal
    | -- | Function application
      App (Expr v) (Expr v)
    | -- | Lambda abstraction
      Lam v (Expr v)
    | -- | Let binding
      Let v (Expr v) (Expr v)
    | -- | Pattern matching
      Match (Expr v) [Case v]
    | -- | Type values. Only used for type applications.
      Type Type
    deriving (Show)

-- | Core type type
data Type
    = TypeVar (Unique VarName)
    deriving (Show)

-- | A case in a pattern match
data Case v
    = Case
        Pattern
        -- ^ Pattern to match
        [Unique VarName]
        -- ^ Bound variables
        (Expr v)
        -- ^ The expression to execute if the pattern matches
    deriving (Show)

-- | Core pattern type
data Pattern
    = -- | Literal pattern
      LitPattern Literal
    | -- | Variable pattern
      VarPattern (Unique VarName)
    | -- | Ignore pattern (@_@)
      IgnorePattern
    deriving (Show)

data Literal
    = CharLit Char
    | StringLit Text
    | IntLit Integer -- TODO: bigger/smaller ints?
    | FloatLit Double
    | UnitLit
    deriving (Show)

instance (Pretty v) => Pretty (Expr v) where
    pretty (Var v) = pretty v
    pretty (Lit l) = pretty l
    pretty (App f x) = pretty f <> " " <> pretty x
    pretty (Lam v e) = "\\" <> pretty v <> " -> " <> pretty e
    pretty (Let v e1 e2) = "let " <> pretty v <> " = " <> pretty e1 <> " in " <> pretty e2
    pretty (Match e cs) = "match " <> pretty e <> " with " <> pretty cs
    pretty (Type t) = pretty t

instance Pretty Literal where
    pretty (CharLit c) = "'" <> pretty c <> "'"
    pretty (StringLit s) = "\"" <> pretty s <> "\""
    pretty (IntLit i) = pretty i
    pretty (FloatLit f) = pretty f
    pretty UnitLit = "()"

instance (Pretty v) => Pretty (Case v) where
    pretty (Case p vs e) = pretty p <> " -> " <> pretty e

instance Pretty Pattern where
    pretty (LitPattern l) = pretty l
    pretty (VarPattern v) = pretty v
    pretty IgnorePattern = "_"

instance Pretty Type where
    pretty (TypeVar v) = pretty v

instance Pretty v => Pretty (VarRef v) where
    pretty (Local v) = pretty v
    pretty (Global v) = pretty v

instance Pretty Var where
    pretty (Id v t) = pretty v <+> ":" <+> pretty t
