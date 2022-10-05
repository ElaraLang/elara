module Elara.TypeInfer.Common where
import Data.Map qualified as Map
import Prelude hiding (Type)
import Text.Show qualified
import Elara.Data.Name
import Elara.AST.Typed (Type (..))

newtype TypeEnv = TypeEnv (Map Var Scheme) deriving (Eq, Ord, Show)

type Subst = Map.Map TypeVariable Type
nullSubst :: Subst
nullSubst = Map.empty

type Var = Name

type Constraint = (Type, Type)

newtype TypeVariable = TV Text
  deriving (Eq, Ord)

instance Show TypeVariable where
  show (TV s) = show s

data Scheme = Forall [TypeVariable] Type
  deriving (Eq, Ord)

instance Show Scheme where
  show (Forall [] t) = show t
  show (Forall vars t) = "∀" ++ foldr (\v s -> s ++ " " ++ show v) "" vars ++ "." ++ show t

