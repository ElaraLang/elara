{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Name where

import Control.Lens (Lens', lens, makeFields, makeLenses, makePrisms, view, (^.))
import Data.Data (Data)
import Data.Text qualified as T (intercalate)
import Elara.AST.Region (Located, unlocated)
import Elara.Data.Pretty
import Elara.Data.Unique
import Text.Show (Show (..))
import Prelude hiding (Show, show)

newtype ModuleName = ModuleName (NonEmpty Text)
    deriving (Show, Eq, Ord, Data)

{- | A valid Variable name. This includes anything that could appear in `let [name] = ...`
In other words, a normal alphanumeric name, or a parenthesis wrapped operator name
-}
data VarName
    = -- | A normal alphanumeric name
      NormalVarName LowerAlphaName
    | -- | An operator var name. Note that while in the source code, the name must be surrounded in parentheses, this is not the case in the AST!
      OperatorVarName OpName
    deriving (Ord, Show, Eq, Data)

--
-- Newtype wrappers over 'Data.Text'
--

{- | A lowercase alphanumeric name. Could be used for variables or type variables
Since type variables can't be operators though, we don't use 'VarName' for them
-}
newtype LowerAlphaName = LowerAlphaName Text
    deriving (Ord, Show, Eq, Data, IsString)

newtype TypeName = TypeName Text
    deriving (Ord, Show, Eq, Data, IsString)

newtype OpName = OpName Text
    deriving (Ord, Show, Eq, Data, IsString)

makePrisms ''LowerAlphaName
makePrisms ''TypeName
makePrisms ''OpName

data Name
    = NVarName VarName
    | NTypeName TypeName
    | NOpName OpName
    deriving (Show, Eq, Ord, Data)

type UniqueName = Unique Name

makeLenses ''Name
makePrisms ''Name

class NameLike name where
    -- | Get the name as a Text. This will not include qualification, if present
    nameText :: name -> Text

    -- | Get the full name, including qualification, if present
    fullNameText :: name -> Text
    fullNameText = nameText

    moduleName :: name -> Maybe ModuleName
    moduleName _ = Nothing

class ToName name where
    toName :: name -> Name

instance ToName VarName where
    toName = NVarName

instance ToName TypeName where
    toName = NTypeName

instance ToName OpName where
    toName = NOpName

instance ToName Name where
    toName = identity

instance NameLike VarName where
    nameText (NormalVarName name) = nameText name
    nameText (OperatorVarName name) = nameText name

instance NameLike LowerAlphaName where
    nameText (LowerAlphaName name) = name

instance NameLike TypeName where
    nameText (TypeName name) = name

instance NameLike OpName where
    nameText (OpName name) = name

instance NameLike ModuleName where
    nameText (ModuleName name) = T.intercalate "." (toList name)

instance (NameLike n) => NameLike (MaybeQualified n) where
    nameText (MaybeQualified name _) = nameText name
    fullNameText (MaybeQualified name modName) =
        maybe
            (nameText name)
            (\m -> nameText m <> "." <> nameText name)
            modName

    moduleName (MaybeQualified _ modName) = modName

instance (NameLike n) => NameLike (Qualified n) where
    nameText (Qualified name _) = nameText name
    fullNameText (Qualified name modName) =
        nameText modName <> "." <> nameText name

    moduleName (Qualified _ modName) = Just modName

instance (NameLike n) => NameLike (Unqualified n) where
    nameText (Unqualified name) = nameText name
    fullNameText (Unqualified name) = nameText name

    moduleName _ = Nothing

instance NameLike Name where
    nameText (NVarName name) = nameText name
    nameText (NTypeName name) = nameText name
    nameText (NOpName name) = nameText name

    fullNameText (NVarName name) = fullNameText name
    fullNameText (NTypeName name) = fullNameText name
    fullNameText (NOpName name) = fullNameText name

    moduleName (NVarName name) = moduleName name
    moduleName (NTypeName name) = moduleName name
    moduleName (NOpName name) = moduleName name

instance (NameLike n) => NameLike (Located n) where
    nameText = nameText . view unlocated
    fullNameText = fullNameText . view unlocated
    moduleName = moduleName . view unlocated

data MaybeQualified name = MaybeQualified
    { _maybeQualifiedName :: name
    , _maybeQualifiedQualifier :: Maybe ModuleName
    }
    deriving (Ord, Show, Eq, Data, Functor, Foldable, Traversable)

data Qualified name = Qualified
    { _qualifiedName :: name
    , _qualifiedQualifier :: ModuleName
    }
    deriving (Show, Eq, Data, Ord, Functor, Foldable, Traversable)

unqualified :: Lens' (Qualified name) name
unqualified =
    lens
        _qualifiedName
        (\q n -> q{_qualifiedName = n})

newtype Unqualified name = Unqualified
    { _unqualifiedName :: name
    }
    deriving (Show, Eq, Data, Ord, Functor, Foldable, Traversable)

makeFields ''MaybeQualified
makeFields ''Qualified
makeFields ''Unqualified
makePrisms ''Unqualified

instance {-# OVERLAPPABLE #-} (Pretty x) => Pretty (MaybeQualified x) where
    pretty (MaybeQualified n (Just m)) = pretty m <> "." <> pretty n
    pretty (MaybeQualified n Nothing) = pretty n

instance {-# OVERLAPPABLE #-} (Pretty x) => Pretty (Qualified x) where
    pretty (Qualified n m) = pretty m <> "." <> pretty n

instance {-# OVERLAPPABLE #-} (Pretty x) => Pretty (Unqualified x) where
    pretty uq = pretty (uq ^. name)

instance Pretty Name where
    pretty (NVarName n) = pretty n
    pretty (NTypeName n) = pretty n
    pretty (NOpName n) = pretty n

instance Pretty ModuleName where
    pretty (ModuleName m) = moduleNameStyle (hcat (punctuate "." (fmap pretty (toList m))))

instance Pretty VarName where
    pretty (NormalVarName n) = varName (pretty n)
    pretty (OperatorVarName n) = "(" <> pretty n <> ")"

instance Pretty (MaybeQualified VarName) where
    pretty (MaybeQualified (OperatorVarName n) (Just q)) = "(" <> pretty q <> "." <> pretty n <> ")"
    pretty (MaybeQualified n (Just q)) = pretty q <> "." <> pretty n
    pretty (MaybeQualified n Nothing) = pretty n

instance Pretty TypeName where
    pretty (TypeName n) = pretty n

instance Pretty OpName where
    pretty (OpName n) = pretty n

instance Pretty LowerAlphaName where
    pretty (LowerAlphaName n) = pretty n