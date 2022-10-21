module Elara.TypeInfer.Common where

import Data.Data (Data)
import Data.Map qualified as Map
import Elara.Data.Name
import Elara.Data.Qualifications (Qualified)
import Text.Show qualified
import Prelude hiding (Type)

newtype TypeEnv = TypeEnv (Map Name Scheme) deriving (Eq, Ord, Show)

type Subst = Map.Map TypeVariable Type

nullSubst :: Subst
nullSubst = Map.empty


type Constraint = (Type, Type)

newtype TypeVariable = TV Text
  deriving (Eq, Ord, Data)

instance Show TypeVariable where
  show (TV s) = toString s

data Scheme = Forall [TypeVariable] Type
  deriving (Eq, Ord, Data)

instance Show Scheme where
  show (Forall [] t) = show t
  show (Forall vars t) = "∀" <> foldr (\v s -> s <> " " <> show v) "" vars <> ". " <> show t

data Type
  = TypeVar Text
  | Type :-> Type
  | TypeConstructorApplication {_constructor :: Type, _args :: [Type]}
  | UserDefinedType
      { _qualified :: Qualified
      , _name :: Name
      }
  deriving (Eq, Ord, Data)

instance Show Type where
  show (TypeVar s) = toString s
  show (t1 :-> t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
  show (TypeConstructorApplication t1 t2) = show t1 ++ " " ++ foldr (\t s -> s ++ " " ++ show t) "" t2
  show (UserDefinedType q n) = show q ++ "." ++ show n