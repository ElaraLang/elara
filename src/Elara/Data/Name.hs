module Elara.Data.Name where

import Data.Text qualified as T

data Name
  = Name T.Text
  | QualifiedName
      { _moduleName :: Maybe ModuleName,
        _name :: Name
      }
  deriving (Show, Ord, Eq)

nameQualification :: Name -> Maybe ModuleName
nameQualification (QualifiedName {_moduleName = m}) = m
nameQualification _ = Nothing

newtype ModuleName = ModuleName [T.Text]
  deriving (Show, Ord, Eq)

class Named a where
  toText :: a -> T.Text

instance Named Name where
  toText (Name n) = n
  toText (QualifiedName _ n) = toText n

instance Named ModuleName where
  toText (ModuleName n) = T.intercalate "." n

class NameFromText a where
  fromText :: T.Text -> a

instance NameFromText Name where
  fromText = Name

instance NameFromText ModuleName where
  fromText txt = ModuleName (T.splitOn "." txt)

fromString :: (NameFromText a) => String -> a
fromString = fromText . T.pack