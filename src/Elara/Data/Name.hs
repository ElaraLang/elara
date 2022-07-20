module Elara.Data.Name where

import Data.Text qualified as T

data Name
  = Name T.Text
  | Qualified QualifiedName
  deriving (Show, Ord, Eq)

data QualifiedName = QualifiedName
  { _moduleName :: Maybe ModuleName,
    _name :: Name
  }
  deriving (Show, Ord, Eq)

class NameLike a where
  nameValue :: a -> T.Text
  nameModule :: a -> Maybe ModuleName
  fullName :: a -> T.Text

instance NameLike Name where
  nameValue (Name _name) = _name
  nameValue (Qualified q) = nameValue q
  nameModule _ = Nothing
  fullName = nameValue

instance NameLike QualifiedName where
  nameValue = nameValue . _name
  nameModule = _moduleName
  fullName qn = case qn._moduleName of
    Nothing -> nameValue qn
    Just mn -> T.concat [fullName mn, ".", nameValue qn]

newtype ModuleName = ModuleName [T.Text]
  deriving (Show, Ord, Eq)

instance NameLike ModuleName where
  nameValue (ModuleName _name) = T.intercalate "." _name
  nameModule _ = Nothing
  fullName = nameValue

class NameFromText a where
  fromText :: T.Text -> a

instance NameFromText Name where
  fromText = Name

instance NameFromText ModuleName where
  fromText txt = ModuleName (T.splitOn "." txt)

fromString :: (NameFromText a) => String -> a
fromString = fromText . T.pack