{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Elara.AST.Region (SourceRegion (..), Located (..), getLocation, unlocate, merge, enclosingRegion, spanningRegion, _SourceRegion, _Unlocate) where

import Control.Lens (makeLenses, Lens)
import Data.Data (Data)
import GHC.Exts (the)
import Relude.Extra

data SourceRegion = SourceRegion
    { sourceFile :: Maybe FilePath
    , startOffset :: Int
    , endOffset :: Int
    }
     deriving (Show, Eq, Ord, Data)

data Located a = Located SourceRegion a
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

_SourceRegion :: Lens' (Located a) SourceRegion
_SourceRegion f (Located region x) = fmap (`Located` x) (f region)

_Unlocate :: Lens (Located a) (Located b) a b
_Unlocate f (Located region x) = fmap (Located region) (f x)


getLocation :: Located a -> SourceRegion
getLocation (Located region _) = region

unlocate :: Located a -> a
unlocate (Located _ x) = x

merge :: (Located a -> Located b -> c) -> Located a -> Located b -> Located c
merge fn l1 l2 =
    Located
        (spanningRegion (getLocation l1 :| [getLocation l2]))
        (fn l1 l2)

-- | Get the region that contains both of the given regions. This function will throw an error if the regions are in different files.
enclosingRegion :: SourceRegion -> SourceRegion -> SourceRegion
enclosingRegion (SourceRegion fp _ _) (SourceRegion fp' _ _) | fp /= fp' = error "enclosingRegion: regions are in different files"
enclosingRegion (SourceRegion fp start _) (SourceRegion _ _ end) = SourceRegion fp start end

-- | Get the region that contains all of the given regions. This function will throw an error if the regions are in different files.
spanningRegion :: NonEmpty SourceRegion -> SourceRegion
spanningRegion regions =
    SourceRegion
        { sourceFile = the $ toList (sourceFile <$> regions)
        , startOffset = minimum1 (startOffset <$> regions)
        , endOffset = maximum1 (endOffset <$> regions)
        }
