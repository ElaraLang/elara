module Elara.Parse.Error where

import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec
import Text.Megaparsec.Error

import Data.Foldable (Foldable (foldl))
import Data.List (lines)
import Data.Set qualified as Set (toList)
import Text.Megaparsec qualified as MP
import Prelude hiding (error, lines)
import Error.Diagnose.Position
import Data.Text qualified as T

data ElaraParseError
    = KeywordUsedAsName Text
    deriving (Eq, Show, Ord)

instance HasHints ElaraParseError Text where
    hints (KeywordUsedAsName kw) =
        [ Note (kw <> " is a keyword which can only be used in certain contexts. However, it was used as a name here.")
        , Hint "Try using a different name"
        ]

class ErrorRegionSize e where
    errorRegion :: e -> Int

instance ErrorRegionSize ElaraParseError where
    errorRegion (KeywordUsedAsName kw) = T.length kw

-- instance ErrorRegion (ParseErrorBundle )

instance ShowErrorComponent ElaraParseError where
    showErrorComponent (KeywordUsedAsName kw) = "Keyword " <> show kw <> " used as name"

