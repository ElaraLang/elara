module Elara.Emit.Operator where

import Data.Text qualified as T

type OpTranslation = (Text, Text)

translations :: [OpTranslation]
translations =
    []

translateOperatorName :: Text -> Text
translateOperatorName name = foldr (uncurry T.replace) name translations
