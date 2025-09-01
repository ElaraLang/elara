{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Name where

import Data.Aeson (ToJSON)
import Data.Data (Data)
import Data.Generics.Product
import Data.Text qualified as T (intercalate)
import Elara.AST.Region (Located, unlocated)
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style
import Elara.Data.Unique
import Text.Show (Show (..))
import Prelude hiding (Show, show)

newtype ModuleName = ModuleName (NonEmpty Text)
    deriving (Show, Eq, Ord, Data, Generic)

appendModule :: ModuleName -> Text -> ModuleName
appendModule (ModuleName m) n = ModuleName (m <> pure n)

{- | A valid Variable name. This includes anything that could appear in `let [name] = ...`
In other words, a normal alphanumeric name, or a parenthesis wrapped operator name
-}
data VarName
    = -- | A normal alphanumeric name
      NormalVarName LowerAlphaName
    | -- | An operator var name. Note that while in the source code, the name must be surrounded in parentheses, this is not the case in the AST!
      OperatorVarName OpName
    deriving (Ord, Show, Eq, Data, Generic)

instance IsString VarName where
    fromString = NormalVarName . fromString

--
-- Newtype wrappers over 'Data.Text'
--

{- | A lowercase alphanumeric name. Could be used for variables or type variables
Since type variables can't be operators though, we don't use 'VarName' for them
-}
newtype LowerAlphaName = LowerAlphaName Text
    deriving (Ord, Show, Eq, Data, IsString, Generic)

newtype TypeName = TypeName Text
    deriving (Ord, Show, Eq, Data, IsString, Generic)

newtype OpName = OpName Text
    deriving (Ord, Show, Eq, Data, IsString, Generic)

data VarOrConName
    = VarName LowerAlphaName
    | ConName TypeName
    deriving (Ord, Show, Eq, Data, Generic)

data Name
    = NVarName VarName
    | NTypeName TypeName
    deriving (Show, Eq, Ord, Data, Generic)

type UniqueName = Unique Name

class NameLike name where
    -- | Get the name as a Text. This will not include qualification, if present
    nameText :: name -> Text

    -- | Get the full name, including qualification, if present
    fullNameText :: name -> Text

    moduleName :: name -> Maybe ModuleName
    moduleName _ = Nothing

class ToName name where
    toName :: name -> Name

instance ToName VarOrConName where
    toName (VarName n) = NVarName (NormalVarName n)
    toName (ConName n) = NTypeName n
instance ToName VarName where
    toName = NVarName

instance ToName TypeName where
    toName = NTypeName

instance ToName OpName where
    toName = NVarName . OperatorVarName

instance ToName Name where
    toName = identity

instance ToName n => ToName (MaybeQualified n) where
    toName (MaybeQualified n _) = toName n

instance ToName n => ToName (Qualified n) where
    toName (Qualified n _) = toName n

instance NameLike VarName where
    nameText (NormalVarName name) = nameText name
    nameText (OperatorVarName name) = nameText name
    fullNameText = nameText

instance NameLike LowerAlphaName where
    nameText (LowerAlphaName name) = name
    fullNameText = nameText

instance NameLike TypeName where
    nameText (TypeName name) = name
    fullNameText = nameText

instance NameLike OpName where
    nameText (OpName name) = name
    fullNameText = nameText

instance NameLike ModuleName where
    nameText (ModuleName name) = T.intercalate "." (toList name)
    fullNameText = nameText

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

instance NameLike Name where
    nameText (NVarName name) = nameText name
    nameText (NTypeName name) = nameText name

    fullNameText (NVarName name) = fullNameText name
    fullNameText (NTypeName name) = fullNameText name

    moduleName (NVarName name) = moduleName name
    moduleName (NTypeName name) = moduleName name

instance NameLike VarOrConName where
    nameText (VarName name) = nameText name
    nameText (ConName name) = nameText name

    fullNameText (VarName name) = fullNameText name
    fullNameText (ConName name) = fullNameText name

    moduleName (VarName name) = moduleName name
    moduleName (ConName name) = moduleName name

instance NameLike n => NameLike (Located n) where
    nameText = nameText . view unlocated
    fullNameText = fullNameText . view unlocated
    moduleName = moduleName . view unlocated

data MaybeQualified name = MaybeQualified
    { _maybeQualifiedName :: name
    , _maybeQualifiedQualifier :: Maybe ModuleName
    }
    deriving (Ord, Show, Eq, Data, Functor, Foldable, Traversable, Generic)

data Qualified name = Qualified
    { _qualifiedName :: name
    , qualifier :: ModuleName
    }
    deriving (Show, Eq, Data, Ord, Generic, Functor, Foldable, Traversable)

unqualified :: Lens' (Qualified name) name
unqualified =
    lens
        _qualifiedName
        (\q n -> q{_qualifiedName = n})

newtype Unqualified name = Unqualified
    { _unqualifiedName :: name
    }
    deriving (Show, Eq, Data, Ord, Functor, Foldable, Traversable, Generic)

instance {-# OVERLAPPABLE #-} Pretty x => Pretty (MaybeQualified x) where
    pretty (MaybeQualified n (Just m)) = pretty m <> "." <> pretty n
    pretty (MaybeQualified n Nothing) = pretty n

instance {-# OVERLAPPABLE #-} Pretty x => Pretty (Qualified x) where
    pretty (Qualified n m) = pretty m <> "." <> pretty n

instance {-# OVERLAPPABLE #-} Pretty x => Pretty (Unqualified x) where
    pretty uq = pretty (uq ^. name)

instance Pretty Name where
    pretty (NVarName n) = pretty n
    pretty (NTypeName n) = pretty n

instance Pretty VarOrConName where
    pretty (VarName n) = pretty n
    pretty (ConName n) = pretty n

instance Pretty ModuleName where
    pretty (ModuleName m) = Style.moduleName (hcat (punctuate "." (fmap pretty (toList m))))

instance Pretty VarName where
    pretty (NormalVarName n) = Style.varName (pretty n)
    pretty (OperatorVarName n) = "(" <> pretty n <> ")"

instance Pretty (MaybeQualified VarName) where
    pretty (MaybeQualified (OperatorVarName n) (Just q)) = "(" <> pretty q <> "." <> pretty n <> ")"
    pretty (MaybeQualified n (Just q)) = pretty q <> "." <> pretty n
    pretty (MaybeQualified n Nothing) = pretty n

instance Pretty TypeName where
    pretty (TypeName n) = pretty n

instance Pretty OpName where
    pretty (OpName n) = Style.operator (pretty n)

instance Pretty LowerAlphaName where
    pretty (LowerAlphaName n) = pretty n

instance ToJSON n => ToJSON (Qualified n)

instance ToJSON ModuleName

instance ToJSON VarName

instance ToJSON OpName

instance ToJSON TypeName

instance ToJSON LowerAlphaName

instance ToJSON Name

instance Hashable b => Hashable (Qualified b)

instance Hashable ModuleName

class HasName a n | a -> n where
    name :: Lens' a n

instance HasName (MaybeQualified n) n where
    name = field @"_maybeQualifiedName"

instance HasName (Qualified n) n where
    name = field @"_qualifiedName"

instance HasName (Unqualified n) n where
    name = field @"_unqualifiedName"

instance HasName Name Name where
    name = lensVL identity

class ContainsName a n | a -> n where
    containedName :: Getter a n

-- instance HasName a n => ContainsName a n where
--     containedName = castOptic name
