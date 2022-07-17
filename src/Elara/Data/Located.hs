module Elara.Data.Located where

{-
Stores location metadata about where something is in the source code.
Used for nice error messages.
-}

data Located expr = Located Region expr
  deriving (Show)

-- Region in the source code. This is calculated as an offset for efficiency.
data Region = Region
  { startOffset :: Int,
    endOffset :: Int
  }
  deriving (Show)

located :: Region -> expr -> Located expr
located = Located

getRegion :: Located expr -> Region
getRegion (Located r _) = r

-- fromSourcePos :: SourcePos -> Region

unlocate :: Located expr -> expr
unlocate (Located _ expr) = expr

instance Functor Located where
  fmap f (Located region expr) = Located region (f expr)

merge :: (Located a -> Located b -> c) -> Located a -> Located b -> Located c
merge fn l1 l2 =
  Located
    (spanningRegion [getRegion l1, getRegion l2])
    (fn l1 l2)

spanningRegion :: [Region] -> Region
spanningRegion regions =
  Region
    { startOffset = minimum (map startOffset regions),
      endOffset = maximum (map endOffset regions)
    }