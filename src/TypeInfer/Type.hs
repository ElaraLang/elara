module TypeInfer.Type where

import Data.Text (Text, unpack)

newtype TVar = TV Text
  deriving (Eq, Ord)

instance Show TVar where
  show (TV s) = unpack s

data Type
  = TVariable TVar
  | TCon Text -- Type constructor
  | TApp Type Type -- Type constructor application
  | TFunc Type Type -- Function type
  deriving (Eq, Ord)

instance Show Type where
  show (TVariable t) = show t
  show (TCon s) = unpack s
  show (TApp t1 t2) = show t1 ++ " " ++ show t2
  show (TFunc t1 t2) = show t1 ++ " -> " ++ show t2

data Scheme = Forall [TVar] Type
  deriving (Eq, Ord)

instance Show Scheme where
  show (Forall [] t) = show t
  show (Forall vars t) = "forall " ++ unwords (map show vars) ++ ". " ++ show t