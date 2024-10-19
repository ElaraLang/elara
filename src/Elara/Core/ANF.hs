-- | Core, but in ANF
module Elara.Core.ANF where

import Data.Data (Data)
import Elara.Core (AltCon, Literal, Type)
import Prelude hiding (Alt)

data AExpr b
    = Var b
    | Lit Literal
    | Lam b (Expr b)
    | TyApp (AExpr b) Type
    | TyLam Type (AExpr b)
    deriving (Show, Eq, Data, Functor, Foldable, Traversable, Typeable, Generic)

data CExpr b
    = App (AExpr b) (AExpr b)
    | Match (AExpr b) (Maybe b) [Alt b]
    deriving (Show, Eq, Data, Functor, Foldable, Traversable, Typeable, Generic)

data Expr b
    = Let (Bind b) (Expr b)
    | CExpr (CExpr b)
    | AExpr (AExpr b)
    deriving (Show, Eq, Data, Functor, Foldable, Traversable, Typeable, Generic)

data Bind b
    = Recursive [(b, Expr b)]
    | NonRecursive (b, Expr b)
    deriving (Show, Eq, Data, Functor, Foldable, Traversable, Generic)

type Alt b = (AltCon, [b], Expr b)