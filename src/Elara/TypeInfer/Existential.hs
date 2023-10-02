{-# LANGUAGE DerivingStrategies #-}

module Elara.TypeInfer.Existential where

import Control.Lens ((^.))
import Data.Aeson (ToJSON)
import Data.Data (Data)
import Data.Text qualified as Text
import Elara.Data.Pretty (Pretty (..))
import Elara.Data.Unique
import Elara.TypeInfer.Unique (UniqueTyVar)

{- | An existential variable
   The type variable is used to track what type of existential variable we're
   using, which will be one of these three types:
   * @`Existential` "Grace.Monotype".Monotype@ - An existential type
   * @`Existential` "Grace.Monotype".Record@ - An existential fields variable
   * @`Existential` "Grace.Monotype".Union@ - An existential alternatives
     variable
-}
newtype Existential a = UnsafeExistential UniqueTyVar
    deriving (Eq, Ord, Data)
    deriving newtype (Show, ToJSON)

instance Pretty (Existential a) where
    pretty x = pretty (toVariable x)

{- | Convert an existential variable to a user-friendly `Text`
   representation
-}
toVariable :: Existential a -> Text
toVariable (UnsafeExistential n) =
    case n ^. uniqueVal of
        Just name -> name
        Nothing -> Text.cons prefix suffix
          where
            (q, r) = (n ^. uniqueId) `quotRem` 26

            prefix = chr (ord 'a' + r)

            suffix = if q == 0 then "" else show (q - 1)
