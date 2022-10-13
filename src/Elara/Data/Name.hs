module Elara.Data.Name where

import Data.Data (Data)
import Data.Text as T
import Text.Show (Show (..))
import Prelude hiding (show)

data Name
  = Name Text
  | Qualified QualifiedName
  deriving (Ord, Eq, Data)

instance Show Name where
  show (Name n) = toString n
  show (Qualified q) = show q

instance IsString Name where
  fromString = Name . Prelude.fromString

data QualifiedName = QualifiedName
  { _qualifiedNameModule :: ModuleName
  , _qualifiedNameName :: Name
  }
  deriving (Show, Ord, Eq, Data)

withModule :: Maybe ModuleName -> Name -> Name
withModule Nothing n = n
withModule (Just m) n = Qualified (QualifiedName m n)

class NameLike a where
  nameValue :: a -> Text
  moduleName :: a -> Maybe ModuleName
  fullName :: a -> Text

instance NameLike Name where
  nameValue (Name _name) = _name
  nameValue (Qualified q) = nameValue q
  moduleName (Name _) = Nothing
  moduleName (Qualified q) = moduleName q
  fullName = nameValue

instance NameLike QualifiedName where
  nameValue = nameValue . _qualifiedNameName
  moduleName = Just . _qualifiedNameModule
  fullName qn = T.concat [fullName (qn._qualifiedNameModule), ".", nameValue qn]

newtype ModuleName = ModuleName (NonEmpty Text)
  deriving (Ord, Eq, Data)

instance Show ModuleName where
  show n = toString $ nameValue n

instance NameLike ModuleName where
  nameValue (ModuleName _name) = T.intercalate "." (toList _name)
  moduleName _ = Nothing
  fullName = nameValue

class NameFromText a where
  fromText :: Text -> a

instance NameFromText Name where
  fromText = Name

instance NameFromText ModuleName where
  fromText txt = ModuleName $ fromList (splitOn "." txt)

fromString :: (NameFromText a) => String -> a
fromString = fromText . toText