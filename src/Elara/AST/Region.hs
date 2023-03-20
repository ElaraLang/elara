{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Elara.AST.Region where

import Control.Lens
import Data.Data (Data)
import Error.Diagnose.Position qualified as Diag
import GHC.Exts (the)
import Text.Megaparsec (SourcePos (SourcePos, sourceColumn, sourceLine, sourceName), mkPos, unPos)
import Text.Show (Show (show))

generatedFileName :: String
generatedFileName = "<generated>"

data RealPosition = Position
    { line :: Int
    , column :: Int
    }
    deriving (Show, Eq, Ord, Data)

data Position
    = RealPosition !RealPosition
    | GeneratedPosition
    deriving (Show, Eq, Ord, Data)

data RealSourceRegion = SourceRegion
    { _sourceFile :: !(Maybe FilePath)
    , _startPos :: !RealPosition
    , _endPos :: !RealPosition
    }
    deriving (Show, Eq, Ord, Data)

data SourceRegion
    = RealSourceRegion !RealSourceRegion
    | GeneratedRegion !FilePath
    deriving (Show, Eq, Ord, Data)

makePrisms ''Position
makeLenses ''RealPosition
makePrisms ''SourceRegion
makeLenses ''RealSourceRegion

class HasPath a where
    path :: Lens' a (Maybe FilePath)

instance HasPath SourceRegion where
    path = lens getter setter
      where
        getter (RealSourceRegion (SourceRegion fp _ _)) = fp
        getter (GeneratedRegion fp) = Just fp
        setter (RealSourceRegion (SourceRegion _ start end)) fp = RealSourceRegion (SourceRegion fp start end)
        setter (GeneratedRegion _) fp = GeneratedRegion (fromMaybe generatedFileName fp)

instance HasPath RealSourceRegion where
    path = lens getter setter
      where
        getter (SourceRegion fp _ _) = fp
        setter (SourceRegion _ start end) fp = SourceRegion fp start end

mkSourceRegion :: SourcePos -> SourcePos -> RealSourceRegion
mkSourceRegion start end =
    SourceRegion
        { _sourceFile = Just $ the (sourceName <$> [start, end])
        , _startPos = spToPosition start
        , _endPos = spToPosition end
        }

spToPosition :: SourcePos -> RealPosition
spToPosition sp =
    Position
        { line = unPos $ sourceLine sp
        , column = unPos $ sourceColumn sp
        }

positionToSp :: FilePath -> RealPosition -> SourcePos
positionToSp fp (Position line column) =
    SourcePos
        { sourceName = fp
        , sourceLine = mkPos line
        , sourceColumn = mkPos column
        }

generatedSourcePos :: Maybe FilePath -> SourcePos
generatedSourcePos fp =
    SourcePos
        { sourceName = fromMaybe generatedFileName fp
        , sourceLine = mkPos 0
        , sourceColumn = mkPos 0
        }

sourceRegionToDiagnosePosition :: SourceRegion -> Diag.Position
sourceRegionToDiagnosePosition (GeneratedRegion fp) =
    Diag.Position
        { Diag.begin = (0, 0)
        , Diag.end = (0, 0)
        , Diag.file = fp
        }
sourceRegionToDiagnosePosition (RealSourceRegion (SourceRegion fp (Position startLine startCol) (Position endLine endCol))) =
    Diag.Position
        { Diag.begin = (startLine, startCol)
        , Diag.end = (endLine, endCol)
        , Diag.file = fromMaybe generatedFileName fp
        }

data Located a = Located SourceRegion a
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
makePrisms ''Located

-- | Newtype wrapper for @Located@ that ignores the location information for its instances
newtype IgnoreLocation a = IgnoreLocation (Located a)
    deriving (Functor, Foldable, Traversable)

makePrisms ''IgnoreLocation

instance Eq a => Eq (IgnoreLocation a) where
    IgnoreLocation (Located _ a) == IgnoreLocation (Located _ b) = a == b

instance Ord a => Ord (IgnoreLocation a) where
    IgnoreLocation (Located _ a) `compare` IgnoreLocation (Located _ b) = a `compare` b

instance Show a => Show (IgnoreLocation a) where
    show (IgnoreLocation (Located _ a)) = Text.Show.show a

sourceRegion :: Lens' (Located a) SourceRegion
sourceRegion f (Located region x) = fmap (`Located` x) (f region)

unlocated :: Lens (Located a) (Located b) a b
unlocated f (Located region x) = fmap (Located region) (f x)

merge :: (Located a -> Located b -> c) -> Located a -> Located b -> Located c
merge fn l1 l2 =
    Located
        (spanningRegion' (l1 ^. sourceRegion :| [l2 ^. sourceRegion]))
        (fn l1 l2)

{- | Get the region that contains both of the given regions.
| This function will throw an error if the regions are in different files.
-}
enclosingRegion :: RealSourceRegion -> RealSourceRegion -> RealSourceRegion
enclosingRegion a b | a ^. path /= b ^. path = error "enclosingRegion: regions are in different files"
enclosingRegion (SourceRegion fp start _) (SourceRegion _ _ end) = SourceRegion fp start end

{- | Get the region that contains both of the given regions.
| This function will throw an error if the regions are in different files.
| If either of the given @SourceRegion@s is a @GeneratedRegion@, then the result will be a @GeneratedRegion@.
-}
enclosingRegion' :: SourceRegion -> SourceRegion -> SourceRegion
enclosingRegion' a b | a ^. path /= b ^. path = error "enclosingRegion: regions are in different files"
enclosingRegion' (GeneratedRegion fp) _ = GeneratedRegion fp
enclosingRegion' _ (GeneratedRegion fp) = GeneratedRegion fp
enclosingRegion' (RealSourceRegion a) (RealSourceRegion b) = RealSourceRegion $ enclosingRegion a b

spanningRegion :: NonEmpty RealSourceRegion -> RealSourceRegion
spanningRegion regions = do
    let file = the $ catMaybes $ regions ^.. traverse . path
    let start = minimum1Of (traverse1 . startPos) regions
    let end = maximum1Of (traverse1 . endPos) regions
    SourceRegion (Just file) start end

{- | Get the region that contains all of the given regions.
| This function will throw an error if the regions are in different files.
| If all the given @SourceRegion@s are @GeneratedRegion@s, then the result will be a @GeneratedRegion@. Otherwise, the @GeneratedRegion@s will be ignored.
-}
spanningRegion' :: NonEmpty SourceRegion -> SourceRegion
spanningRegion' regions = do
    let file = the $ catMaybes $ regions ^.. traverse . path
    let realRegions = regions ^.. traverse . _RealSourceRegion
    case nonEmpty realRegions of
        Nothing -> GeneratedRegion file
        Just realRegions' -> RealSourceRegion $ spanningRegion realRegions'
