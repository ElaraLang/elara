module Elara.TypeInfer.TypeVariables where

import Elara.AST.Name (LowerAlphaName (LowerAlphaName))
import Elara.AST.Typed
import Elara.Data.Unique
import Polysemy
import Relude.Unsafe ((!!))

-- | Infinite list of type variable names
letters :: [Text]
letters = toText <$> ([1 ..] >>= flip replicateM ['a' .. 'z'])

-- TODO: make this not O(n)
newTypeVar :: (Member UniqueGen r) => Sem r TypeVar
newTypeVar = do
    id <- newUniqueNum
    let name = letters !! id
    pure $ TyVar (Unique (LowerAlphaName name) id)