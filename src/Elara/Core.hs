module Elara.Core where

import Control.Lens (Plated)
import Data.Data
import Data.Map qualified as Map
import Elara.AST.Name (Qualified)
import Elara.AST.VarRef (UnlocatedVarRef)
import Elara.Data.Kind (ElaraKind)
import Elara.Data.Unique (Unique)
import Elara.TypeInfer.Unique
import Prelude hiding (Alt)

data TypeVariable = TypeVariable
    { tvName :: UniqueTyVar
    , tvKind :: ElaraKind
    }
    deriving (Show, Eq, Data)

data Var
    = TyVar TypeVariable
    | Id
        { idVarName :: UnlocatedVarRef Text
        , idVarType :: Type
        }
    deriving (Show, Data, Eq)

data Expr b
    = Var b
    | Lit Literal
    | App (Expr b) (Expr b)
    | Lam b (Expr b)
    | Let (Bind b) (Expr b)
    | Match (Expr b) (Maybe b) [Alt b]
    | Type Type
    deriving (Show, Eq, Data, Functor, Foldable, Traversable, Typeable)

instance Data b => Plated (Expr b)

type CoreExpr = Expr Var

type CoreAlt = Alt Var

type CoreBind = Bind Var

data Bind b
    = Recursive [(b, Expr b)]
    | NonRecursive (b, Expr b)
    deriving (Show, Eq, Data, Functor, Foldable, Traversable)

binds :: Bind b -> [(b, Expr b)]
binds = \case
    Recursive bs -> bs
    NonRecursive b -> [b]

type Alt b = (AltCon, [b], Expr b)

data AltCon
    = DataAlt DataCon
    | LitAlt Literal
    | DEFAULT
    deriving (Show, Eq, Data)

-- | A data constructor.
data DataCon = DataCon
    { name :: Qualified Text
    , dataConType :: Type
    }
    deriving (Show, Eq, Data)

data Type
    = TyVarTy TypeVariable
    | FuncTy Type Type
    | AppTy Type Type
    | -- | A type constructor
      ConTy (Qualified Text)
    | ForAllTy TypeVariable Type
    deriving (Show, Eq, Data)

data Literal
    = Int Integer
    | String Text
    | Char Char
    | Double Double
    | Unit
    deriving (Show, Eq, Data)
