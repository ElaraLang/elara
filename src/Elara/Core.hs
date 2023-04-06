{- | Elara core language
After type inference, the AST is compiled to this much simpler language,
which makes further stages in the compilation pipeline much easier.
-}
module Elara.Core where

import Elara.AST.Name (Name, Qualified, VarName)
import Elara.Data.Unique

type CoreExpr = Expr VarRef

type CoreCase = Case VarRef

data VarRef
  = Local (Unique VarName)
  | Global (Qualified Name)
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
  deriving (Show)