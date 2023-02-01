module Elara.AST.Region where

import Data.Data (Data)
import Relude.Extra

data SourceRegion = SourceRegion
    { startOffset :: Int
    , endOffset :: Int
    }
    deriving (Show, Eq, Data)

data Located a = Located SourceRegion a
    deriving (Show, Eq)

getLocation :: Located a -> SourceRegion
getLocation (Located region _) = region

unlocate :: Located a -> a
unlocate (Located _ x) = x

merge :: (Located a -> Located b -> c) -> Located a -> Located b -> Located c
merge fn l1 l2 =
    Located
        (spanningRegion (getLocation l1 :| [getLocation l2]))
        (fn l1 l2)

enclosingRegion :: SourceRegion -> SourceRegion -> SourceRegion
enclosingRegion (SourceRegion start _) (SourceRegion _ end) = SourceRegion start end

spanningRegion :: NonEmpty SourceRegion -> SourceRegion
spanningRegion regions =
    SourceRegion
        { startOffset = minimum1 (startOffset <$> regions)
        , endOffset = maximum1 (endOffset <$> regions)
        }
