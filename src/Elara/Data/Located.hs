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
    (mergeRegions (getRegion l1) (getRegion l2))
    (fn l1 l2)

mergeRegions :: Region -> Region -> Region
mergeRegions r1 r2 =
  Region
    { startOffset = minimum [r1.startOffset, r2.startOffset],
      endOffset = maximum [r1.endOffset, r2.endOffset]
    }