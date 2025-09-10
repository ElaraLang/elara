-- | Stores the kind of a type.
module Elara.Data.Kind where

import Data.Aeson (ToJSON)
import Data.Data (Data)
import Elara.AST.Name (LowerAlphaName)
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style
import Elara.Data.Unique

data ElaraKind
    = -- | The kind of monotypes (@Type@ or @*@ in Haskell)
      TypeKind
    | -- | Functions over kinds (eg @Type -> Type@ in Haskell)
      FunctionKind ElaraKind ElaraKind
    | -- | A kind variable for poly-kinds (probably not supported yet)
      VarKind KindVar
    | KindScheme [KindVar] ElaraKind
    deriving (Show, Eq, Data, Ord, Generic)

type KindVar = UniqueId

type TypeVar = Unique LowerAlphaName

instance Pretty ElaraKind where
    pretty TypeKind = Style.typeName "Type"
    pretty (FunctionKind l r) = pretty l <> " -> " <> pretty r
    pretty (VarKind v) = Style.varName ("k" <> pretty v)

instance ToJSON ElaraKind

instance Hashable ElaraKind
