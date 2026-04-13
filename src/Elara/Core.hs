{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.Core where

import Data.Data (Data)
import Elara.AST.Name (Qualified)
import Elara.AST.VarRef (UnlocatedVarRef)
import Elara.Core.Generic qualified as G
import Elara.Data.Kind (ElaraKind)
import Elara.Prim (OpaquePrim)
import Elara.Prim qualified as Prim
import Elara.TypeInfer.Unique
import GHC.Generics (Rep)
import Prelude hiding (Alt)

data TypeVariable = TypeVariable
    { tvName :: UniqueTyVar
    , tvKind :: ElaraKind
    }
    deriving (Show, Eq, Data, Ord, Generic)

data Var
    = TyVar TypeVariable
    | Id (UnlocatedVarRef Text) Type (Maybe DataCon)
    deriving (Show, Data, Generic)

instance Eq Var where
    (TyVar a) == (TyVar b) = a == b
    (Id a _ _) == (Id b _ _) = a == b
    _ == _ = False

instance Ord Var where
    compare (TyVar a) (TyVar b) = compare a b
    compare (Id a _ _) (Id b _ _) = compare a b
    compare (TyVar _) (Id{}) = LT
    compare (Id{}) (TyVar _) = GT

-- instance Hashable Var where
--   hashWithSalt s (TyVar t) = hashWithSalt s t
--   hashWithSalt s (Id v _ _) = hashWithSalt s v

data Expr b
    = Var b
    | Lit Literal
    | App (Expr b) (Expr b)
    | TyApp (Expr b) Type
    | Lam b (Expr b)
    | TyLam Type (Expr b)
    | Let (Bind b) (Expr b)
    | Match (Expr b) (Maybe b) [Alt b]
    | {- | A resolved primitive operation, including its instantiated type.
      Applied via normal 'App' nodes, so currying works naturally.
      -}
      PrimOp Prim.PrimOp Type
    deriving (Show, Eq, Data, Typeable, Generic)

instance Generic b => Plated (Expr b) (Expr b)

type CoreExpr = Expr Var

type CoreAlt = Alt Var

type CoreBind = Bind Var

type Bind b = G.Bind b Expr

type Alt b = (AltCon, [b], Expr b)

data AltCon
    = DataAlt DataCon
    | LitAlt Literal
    | DEFAULT
    deriving (Show, Eq, Data, Generic, Ord)

-- | A data constructor.
data DataCon = DataCon
    { name :: Qualified Text
    -- ^ The name of the data constructor
    , dataConType :: Type
    -- ^ The type of the data constructor, i.e. `type Foo a = Bar a` would have a data constructor with type `a -> Foo a`
    , dataConDataType :: TyCon
    -- ^ The type of the data type the data constructor belongs to, i.e. `type Foo a = Bar a` would have a DataCon for `Bar a` with @dataConDataType = Foo a@. This should be identical to @functionTypeResult . dataConType@
    }
    deriving (Show, Eq, Data, Generic, Ord)

data Type
    = -- | A type variable, @a@
      TyVarTy TypeVariable
    | -- | A function type, @t -> t'@
      FuncTy Type Type
    | -- | An application of a type constructor, @TC t@
      AppTy Type Type
    | -- | A type constructor, @TC@
      ConTy TyCon
    | -- | A forall quantified type, @forall a. T@
      ForAllTy !TypeVariable !Type
    deriving (Show, Eq, Data, Ord, Generic)

-- | Information about a type constructor
data TyCon
    = TyCon
        -- | The name of the type constructor
        (Qualified Text)
        -- | The details of the type constructor
        TyConDetails
    deriving (Show, Eq, Data, Ord, Generic)

-- | The details of a type constructor, mainly about its definition
data TyConDetails
    = -- | An ADT
      TyADT
        -- | The ids of its 'DataCon's (constructors)
        [Qualified Text]
    | TyAlias Type
    | -- | An opaque primitive type backed directly by the backend.
      Prim OpaquePrim
    deriving (Show, Eq, Data, Ord, Generic)

instance
    forall x.
    (Generic x, SafeGPlate (Rep x) Type, GPlate Type x) => Plated Type x

-- | The arity of a function type
typeArity :: Type -> Int
typeArity = \case
    FuncTy _ b -> 1 + typeArity b
    ForAllTy _ t -> typeArity t
    _ -> 0

functionTypeArgs :: Type -> [Type]
functionTypeArgs = \case
    ForAllTy _ t -> functionTypeArgs t
    FuncTy a b -> a : functionTypeArgs b
    _ -> []

conTyArgs :: Type -> [Type]
conTyArgs = \case
    AppTy ConTy{} b -> [b]
    AppTy tc a -> conTyArgs tc ++ [a]
    _ -> []

functionTypeResult :: Type -> Type
functionTypeResult = \case
    ForAllTy _ t -> functionTypeResult t
    FuncTy _ b -> functionTypeResult b
    t -> t

instantiate :: Type -> Type -> Type
instantiate (ForAllTy tv t) t' = substTypeVar tv t' t
instantiate t _ = t

substTypeVar :: TypeVariable -> Type -> Type -> Type
substTypeVar tv replaceWith = transform f
  where
    f = \case
        TyVarTy tv' | tv == tv' -> replaceWith
        other -> other

data Literal
    = Int !Integer
    | String !Text
    | Char !Char
    | Double !Double
    | Unit
    deriving (Show, Eq, Data, Generic, Ord)

-- instance (Hashable b) => Hashable (Expr b)

-- instance (Hashable b) => Hashable (Bind b)

instance Hashable Literal

-- instance Hashable AltCon

-- instance Hashable DataCon

-- instance Hashable Type

-- instance Hashable TyCon

-- instance Hashable TyConDetails

-- instance Hashable TypeVariable
