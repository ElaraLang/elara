module Elara.Core where

import Control.Lens (Plated)
import Data.Data
import Elara.AST.Name (Qualified)
import Elara.AST.VarRef (UnlocatedVarRef)
import Elara.Data.Kind (ElaraKind)
import Elara.TypeInfer.Unique
import Relude.Extra (bimapF)
import Prelude hiding (Alt)
import Control.Monad.State hiding (StateT)

data TypeVariable = TypeVariable
    { tvName :: UniqueTyVar
    , tvKind :: ElaraKind
    }
    deriving (Show, Eq, Data, Ord, Generic)

data Var
    = TyVar TypeVariable
    | Id
        { idVarName :: UnlocatedVarRef Text
        , idVarType :: Type
        }
    deriving (Show, Data, Eq, Ord, Generic)

data Expr b
    = Var b
    | Lit Literal
    | App (Expr b) (Expr b)
    | TyApp (Expr b) Type
    | Lam b (Expr b)
    | TyLam Type (Expr b)
    | Let (Bind b) (Expr b)
    | Match (Expr b) (Maybe b) [Alt b]
    deriving (Show, Eq, Data, Functor, Foldable, Traversable, Typeable, Generic)

instance (Data b) => Plated (Expr b)

type CoreExpr = Expr Var

type CoreAlt = Alt Var

type CoreBind = Bind Var

data Bind b
    = Recursive [(b, Expr b)]
    | NonRecursive (b, Expr b)
    deriving (Show, Eq, Data, Functor, Foldable, Traversable, Generic)

binds :: Bind b -> [(b, Expr b)]
binds = \case
    Recursive bs -> bs
    NonRecursive b -> [b]

mapBind :: (b -> b') -> (Expr b -> Expr b') -> Bind b -> Bind b'
mapBind f g = \case
    Recursive bs -> Recursive (bimapF f g bs)
    NonRecursive (b, e) -> NonRecursive (f b, g e)

type Alt b = (AltCon, [b], Expr b)




data AltCon
    = DataAlt DataCon
    | LitAlt Literal
    | DEFAULT
    deriving (Show, Eq, Data, Generic)

-- | A data constructor.
data DataCon = DataCon
    { name :: Qualified Text
    , dataConType :: Type
    }
    deriving (Show, Eq, Data, Generic)

data Type
    = TyVarTy TypeVariable
    | FuncTy Type Type
    | AppTy Type Type
    | -- | A type constructor
      ConTy (Qualified Text)
    | ForAllTy TypeVariable Type
    deriving (Show, Eq, Data, Ord, Generic)

-- | The arity of a function type
typeArity :: Type -> Int
typeArity = \case
    FuncTy _ b -> 1 + typeArity b
    _ -> 0

instantiate :: Type -> Type -> Type
instantiate (ForAllTy tv t) t' = instantiate t (substTypeVar tv t')
instantiate t _ = t

substTypeVar :: TypeVariable -> Type -> Type
substTypeVar tv = \case
    TyVarTy tv' | tv == tv' -> TyVarTy tv
    FuncTy a b -> FuncTy (substTypeVar tv a) (substTypeVar tv b)
    AppTy a b -> AppTy (substTypeVar tv a) (substTypeVar tv b)
    ConTy n -> ConTy n
    ForAllTy tv' t | tv /= tv' -> ForAllTy tv' (substTypeVar tv t)
    ForAllTy tv' t | tv == tv' -> ForAllTy tv' t
    other -> error $ "substTypeVar: " <> show other

data Literal
    = Int Integer
    | String Text
    | Char Char
    | Double Double
    | Unit
    deriving (Show, Eq, Data, Generic)

instance (Hashable b) => Hashable (Expr b)

instance (Hashable b) => Hashable (Bind b)

instance Hashable Literal

instance Hashable AltCon

instance Hashable DataCon

instance Hashable Type

instance Hashable TypeVariable

instance Hashable Var
