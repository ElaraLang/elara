{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Name where

import Control.Lens (makeClassy)
import Data.Data (Data)
import Data.Text qualified as T (intercalate)

newtype ModuleName = ModuleName (NonEmpty Text)
    deriving (Show, Eq, Ord, Data)

newtype VarName = VarName Text
    deriving (Ord, Show, Eq, Data)

newtype TypeName = TypeName Text
    deriving (Ord, Show, Eq, Data)

newtype OpName = OpName Text
    deriving (Ord, Show, Eq, Data)

data Name qual
    = NVarName (qual VarName)
    | NTypeName (qual TypeName)
    | NOpName OpName

deriving instance (Show (qual VarName), Show (qual TypeName)) => Show (Name qual)
deriving instance (Eq (qual VarName), Eq (qual TypeName)) => Eq (Name qual)
deriving instance (Typeable qual, Data (qual VarName), Data (qual TypeName)) => Data (Name qual)

class NameLike name where
    -- | Get the name as a Text. This will not include qualification, if present
    nameText :: name -> Text

    -- | Get the full name, including qualification, if present
    fullNameText :: name -> Text
    fullNameText = nameText

instance NameLike VarName where
    nameText (VarName name) = name

instance NameLike TypeName where
    nameText (TypeName name) = name

instance NameLike OpName where
    nameText (OpName name) = name

instance NameLike ModuleName where
    nameText (ModuleName name) = T.intercalate "." (toList name)

instance NameLike n => NameLike (MaybeQualified n) where
    nameText (MaybeQualified name _) = nameText name
    fullNameText (MaybeQualified name moduleName) =
        maybe
            (nameText name)
            (\m -> nameText m <> "." <> nameText name)
            moduleName

data MaybeQualified name = MaybeQualified
    { _maybeQualifiedNameName :: name
    , _maybeQualifiedNameQualifier :: Maybe ModuleName
    }
    deriving (Ord, Show, Eq, Data)

data Qualified name = Qualified
    { _qualifiedNameName :: name
    , _qualifiedNameQualifier :: ModuleName
    }
    deriving (Show, Eq)

makeClassy ''MaybeQualified
makeClassy ''Qualified
