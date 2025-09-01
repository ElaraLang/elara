-- | Core, but in ANF
module Elara.Core.ANF where

import Data.Data (Data)
import Elara.Core (AltCon, Literal, Type)
import Elara.Core.Generic qualified as G
import Prelude hiding (Alt, group)

-- | An atomic expression
data AExpr b
    = Var b
    | Lit Literal
    | Lam b (Expr b)
    | TyApp (AExpr b) Type
    | TyLam Type (AExpr b)
    deriving (Show, Eq, Data, Typeable, Generic)

-- | A combinator expression
data CExpr b
    = App (AExpr b) (AExpr b)
    | AExpr (AExpr b)
    | Match (AExpr b) (Maybe b) [Alt b]
    deriving (Show, Eq, Data, Typeable, Generic)

-- | A "top level" expression, which is either a let binding or a CExpr
data Expr b
    = Let (Bind b) (Expr b)
    | CExpr (CExpr b)
    deriving (Show, Eq, Data, Typeable, Generic)

type Bind b = G.Bind b CExpr

type TopLevelBind b = G.Bind b Expr

type Alt b = (AltCon, [b], Expr b)
