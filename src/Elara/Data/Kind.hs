-- | Stores the kind of a type.
module Elara.Data.Kind where

import Elara.AST.Name (LowerAlphaName)
import Elara.Data.Pretty
import Elara.Data.Unique

data ElaraKind
    = -- | The kind of monotypes (@Type@ or @*@ in Haskell)
      TypeKind
    | -- | Functions over kinds (eg @Type -> Type@ in Haskell)
      FunctionKind ElaraKind ElaraKind
    | -- | A kind variable for poly-kinds (probably not supported yet)
      VarKind (Unique LowerAlphaName)

instance Pretty ElaraKind where
    pretty TypeKind = "Type"
    pretty (FunctionKind l r) = pretty l <> " -> " <> pretty r
    pretty (VarKind v) = pretty v


