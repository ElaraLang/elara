{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Name where

import Control.Lens (makeClassy, makeLenses, makePrisms)
import Data.Data (Data)
import Data.Text qualified as T (intercalate)
import Text.Show (Show (..))
import Prelude hiding (Show, show)

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
    | NOpName (qual OpName)

makeLenses ''Name
makePrisms ''Name

deriving instance (Show (qual VarName), Show (qual TypeName), Show (qual OpName)) => Show (Name qual)
deriving instance (Eq (qual VarName), Eq (qual TypeName), Eq (qual OpName)) => Eq (Name qual)
deriving instance (Ord (qual VarName), Ord (qual TypeName), Ord (qual OpName)) => Ord (Name qual)

class NameLike name where
    -- | Get the name as a Text. This will not include qualification, if present
    nameText :: name -> Text

    -- | Get the full name, including qualification, if present
    fullNameText :: name -> Text
    fullNameText = nameText

    moduleName :: name -> Maybe ModuleName
    moduleName _ = Nothing

class ToName name qual | name -> qual where
    toName :: name -> Name qual

instance ToName (qual VarName) qual where
    toName = NVarName

instance ToName (qual TypeName) qual where
    toName = NTypeName

instance ToName (qual OpName) qual where
    toName = NOpName

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
    fullNameText (MaybeQualified name modName) =
        maybe
            (nameText name)
            (\m -> nameText m <> "." <> nameText name)
            modName

    moduleName (MaybeQualified _ modName) = modName

instance NameLike n => NameLike (Qualified n) where
    nameText (Qualified name _) = nameText name
    fullNameText (Qualified name modName) =
        nameText modName <> "." <> nameText name

    moduleName (Qualified _ modName) = Just modName

instance NameLike n => NameLike (Unqualified n) where
    nameText (Unqualified name) = nameText name
    fullNameText (Unqualified name) = nameText name

    moduleName _ = Nothing

data MaybeQualified name = MaybeQualified
    { _maybeQualifiedNameName :: name
    , _maybeQualifiedNameQualifier :: Maybe ModuleName
    }
    deriving (Ord, Show, Eq, Data)

data Qualified name = Qualified
    { _qualifiedNameName :: name
    , _qualifiedNameQualifier :: ModuleName
    }
    deriving (Show, Eq, Data, Ord)
newtype Unqualified name = Unqualified
    { _unqualifiedNameName :: name
    }
    deriving (Show, Eq, Data, Ord)

makeClassy ''MaybeQualified
makeClassy ''Qualified
