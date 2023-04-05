module Elara.TypeInfer.Existential where

import Elara.Data.Pretty (Pretty (..))

import Data.Text qualified as Text

{- | An existential variable
    The type variable is used to track what type of existential variable we're
    using, which will be one of these three types:
    * @`Existential` "Grace.Monotype".Monotype@ - An existential type
    * @`Existential` "Grace.Monotype".Record@ - An existential fields variable
    * @`Existential` "Grace.Monotype".Union@ - An existential alternatives
      variable
-}
newtype Existential a = UnsafeExistential Int
    deriving (Eq, Num, Ord, Show)

instance Pretty (Existential a) where
    pretty x = pretty (toVariable x)

{- | Convert an existential variable to a user-friendly `Text`
    representation
    >>> toVariable 0
    "a"
    >>> toVariable 1
    "b"
    >>> toVariable 26
    "a0"
-}
toVariable :: Existential a -> Text
toVariable (UnsafeExistential n) = Text.cons prefix suffix
  where
    (q, r) = n `quotRem` 26

    prefix = chr (ord 'a' + r)

    suffix = if q == 0 then "" else show (q - 1)