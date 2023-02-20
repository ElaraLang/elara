module Elara.Parse.Error where

import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec
import Text.Megaparsec.Error

import Data.Foldable (Foldable (foldl))
import Data.List (lines)
import Data.Set qualified as Set (toList)
import Data.Text qualified as T
import Elara.AST.Region (SourceRegion)
import Elara.Error (ReportableError (reportDiagnostic), report)
import Error.Diagnose.Position
import Text.Megaparsec qualified as MP
import Prelude hiding (error, lines)

data ElaraParseError
    = KeywordUsedAsName Text
    deriving (Eq, Show, Ord)

instance HasHints ElaraParseError Text where
    hints (KeywordUsedAsName kw) =
        [ Note (kw <> " is a keyword which can only be used in certain contexts. However, it was used as a name here.")
        , Hint "Try using a different name"
        ]

instance (HasHints m Text, ShowErrorComponent m, MP.VisualStream e, MP.TraversableStream e) => ReportableError (ParseErrorBundle e m) where
    reportDiagnostic = diagnosticFromBundle (const True) (Just "E0001") "Parse error" Nothing

class ErrorRegionSize e where
    errorRegion :: e -> Int

instance ErrorRegionSize ElaraParseError where
    errorRegion (KeywordUsedAsName kw) = T.length kw

instance ShowErrorComponent ElaraParseError where
    showErrorComponent (KeywordUsedAsName kw) = "Keyword " <> show kw <> " used as name"

-- addPosition :: SourceRegion -> Report msg -> Report msg
-- addPosition sr (Warn code msg poss notes) = Warn code msg (sr : poss) notes