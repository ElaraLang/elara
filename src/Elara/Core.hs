module Elara.Core where

import Data.Data
import Elara.AST.Name (Qualified)
import Elara.AST.VarRef (UnlocatedVarRef)
import Elara.Data.Kind (ElaraKind)
import Elara.TypeInfer.Unique
import Relude.Extra (bimapF)
import Prelude hiding (Alt)

data TypeVariable = TypeVariable
    { tvName :: UniqueTyVar
    , tvKind :: ElaraKind
    }
    deriving (Show, Eq, Data, Ord, Generic)

data Var
    = TyVar TypeVariable
    | Id (UnlocatedVarRef Text) Type
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

instance Plated (Expr b) where
    plate = traversalVL $ \f -> \case
        Var b -> pure (Var b)
        Lit l -> pure (Lit l)
        App a b -> App <$> f a <*> f b
        TyApp a b -> TyApp <$> f a <*> pure b
        Lam b e -> (Lam b <$> f e)
        TyLam t e -> TyLam t <$> f e
        Let b e -> (Let <$> f' b <*> f e)
          where
            f' = \case
                Recursive bs -> Recursive <$> traverse (traverse f) bs
                NonRecursive (b, e) -> NonRecursive <$> ((,) b <$> f e)
        Match e b as -> Match <$> f e <*> pure b <*> traverse (traverse3 f) as
          where
            traverse3 f (a, b, c) = ((,,) a b <$> f c)

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
    | ForAllTy !TypeVariable !Type
    deriving (Show, Eq, Data, Ord, Generic)

instance Plated Type where
    plate = traversalVL $ \f -> \case
        TyVarTy tv -> pure (TyVarTy tv)
        FuncTy a b -> FuncTy <$> f a <*> f b
        AppTy a b -> AppTy <$> f a <*> f b
        ConTy n -> pure (ConTy n)
        ForAllTy tv t -> ForAllTy tv <$> f t

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
    deriving (Show, Eq, Data, Generic)

instance Hashable b => Hashable (Expr b)

instance Hashable b => Hashable (Bind b)

instance Hashable Literal

instance Hashable AltCon

instance Hashable DataCon

instance Hashable Type

instance Hashable TypeVariable

instance Hashable Var
