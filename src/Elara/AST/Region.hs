{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Elara.AST.Region where

import Data.Aeson (ToJSON)
import Data.Data (Data)
import Elara.Data.Pretty (Pretty (..))
import Error.Diagnose.Position qualified as Diag
import GHC.Exts (the)
import Optics (FoldableWithIndex, FunctorWithIndex, TraversableWithIndex, maximumOf, minimumOf)
import Print (showPretty)
import Relude.Unsafe (fromJust)
import Text.Megaparsec (SourcePos (SourcePos, sourceColumn, sourceLine, sourceName), mkPos, unPos)
import Text.Show (Show (show))

generatedFileName :: String
generatedFileName = "<generated>"

data RealPosition = Position
    { _line :: !Int
    , _column :: !Int
    }
    deriving (Show, Eq, Ord, Data, Generic)

data Position
    = RealPosition !RealPosition
    | GeneratedPosition
    deriving (Show, Eq, Ord, Data, Generic)

data RealSourceRegion = SourceRegion
    { _sourceFile :: !(Maybe FilePath)
    , _startPos :: !RealPosition
    , _endPos :: !RealPosition
    }
    deriving (Show, Eq, Ord, Data, Generic)

data SourceRegion
    = RealSourceRegion !RealSourceRegion
    | GeneratedRegion !FilePath
    deriving (Show, Eq, Ord, Data, Generic)

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
        { _line = unPos $ sourceLine sp
        , _column = unPos $ sourceColumn sp
        }

positionToSp :: FilePath -> RealPosition -> SourcePos
positionToSp fp pos =
    SourcePos
        { sourceName = fp
        , sourceLine = mkPos (pos ^. line)
        , sourceColumn = mkPos (pos ^. column)
        }

generatedSourcePos :: Maybe FilePath -> SourcePos
generatedSourcePos fp =
    SourcePos
        { sourceName = fromMaybe generatedFileName fp
        , sourceLine = mkPos 1
        , sourceColumn = mkPos 1
        }

generatedSourceRegion :: Maybe FilePath -> SourceRegion
generatedSourceRegion fp = GeneratedRegion (fromMaybe generatedFileName fp)

generatedLocated :: Maybe FilePath -> a -> Located a
generatedLocated fp = Located (generatedSourceRegion fp)

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

positionToDiagnosePosition :: FilePath -> RealPosition -> Diag.Position
positionToDiagnosePosition fp (Position ln cn) =
    Diag.Position
        { Diag.begin = (ln, cn)
        , Diag.end = (ln, cn + 1)
        , Diag.file = fp
        }

data Located a = Located SourceRegion a
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Data, Generic)

class HasSourceRegion a where
    sourceRegion :: Lens' a SourceRegion

instance HasSourceRegion (Located a) where
    sourceRegion = lensVL $ \f (Located region x) -> fmap (`Located` x) (f region)

instance FoldableWithIndex Int Located
instance TraversableWithIndex Int Located
instance FunctorWithIndex Int Located

instance Each Int (Located a) (Located b) a b

makePrisms ''Located

type family Unlocate g where
    Unlocate (Located a) = a
    Unlocate a = a

-- | Newtype wrapper for 'Located' that ignores the location information for its instances
newtype IgnoreLocation a = IgnoreLocation (Located a)
    deriving (Functor, Foldable, Traversable, Generic)

makePrisms ''IgnoreLocation

generatedSourceRegionFrom :: Located a -> SourceRegion
generatedSourceRegionFrom = generatedSourceRegion . view (sourceRegion % path)

unlocated :: Lens (Located a) (Located b) a b
unlocated = lensVL $ \f (Located region x) -> fmap (Located region) (f x)

withLocationOf :: HasSourceRegion b => a -> b -> Located a
withLocationOf a b = Located (b ^. sourceRegion) a

merge :: (HasSourceRegion a, HasSourceRegion b) => (a -> b -> c) -> a -> b -> Located c
merge fn l1 l2 =
    Located
        (spanningRegion' (l1 ^. sourceRegion :| [l2 ^. sourceRegion]))
        (fn l1 l2)

{- | Get the region that contains both of the given regions.
This function will throw an error if the regions are in different files.
-}
enclosingRegion :: HasCallStack => RealSourceRegion -> RealSourceRegion -> RealSourceRegion
enclosingRegion a b | a ^. path /= b ^. path = error "enclosingRegion: regions are in different files"
enclosingRegion (SourceRegion fp start _) (SourceRegion _ _ end) = SourceRegion fp start end

{- | Get the region that contains both of the given regions.
This function will throw an error if the regions are in different files.
If either of the given 'SourceRegion's is a 'GeneratedRegion', then the result will be a 'GeneratedRegion'.
-}
enclosingRegion' :: HasCallStack => SourceRegion -> SourceRegion -> SourceRegion
enclosingRegion' a b | a ^. path /= b ^. path = error ("enclosingRegion: regions are in different files: " <> showPretty a <> " & " <> showPretty b)
enclosingRegion' (GeneratedRegion fp) _ = GeneratedRegion fp
enclosingRegion' _ (GeneratedRegion fp) = GeneratedRegion fp
enclosingRegion' (RealSourceRegion a) (RealSourceRegion b) = RealSourceRegion $ enclosingRegion a b

spanningRegion :: NonEmpty RealSourceRegion -> RealSourceRegion
spanningRegion regions = do
    let file = the $ catMaybes $ regions ^.. traversed % path
    let start = fromJust $ minimumOf (traversed % startPos) regions
    let end = fromJust $ maximumOf (traversed % endPos) regions
    SourceRegion (Just file) start end

{- | Get the region that contains all of the given regions.
 This function will throw an error if the regions are in different files.
 If all the given 'SourceRegion's are 'GeneratedRegion's, then the result will be a 'GeneratedRegion'. Otherwise, the 'GeneratedRegion's will be ignored.
-}
spanningRegion' :: NonEmpty SourceRegion -> SourceRegion
spanningRegion' regions = do
    let file = the $ catMaybes $ regions ^.. traversed % path
    let realRegions = regions ^.. traversed % _RealSourceRegion
    case nonEmpty realRegions of
        Nothing -> GeneratedRegion file
        Just realRegions' -> RealSourceRegion $ spanningRegion realRegions'

instance Semigroup SourceRegion where
    (<>) = enclosingRegion'

instance Semigroup RealSourceRegion where
    (<>) = enclosingRegion

instance Monoid SourceRegion where
    mempty = GeneratedRegion generatedFileName

instance Applicative Located where
    pure = Located (GeneratedRegion generatedFileName)
    Located region f <*> Located region' x = Located (region <> region') (f x)

instance Pretty a => Pretty (Located a) where
    pretty (Located _ x) = pretty x

instance Pretty a => Pretty (IgnoreLocation a) where
    pretty (IgnoreLocation x) = pretty x

instance Eq a => Eq (IgnoreLocation a) where
    IgnoreLocation (Located _ a) == IgnoreLocation (Located _ b) = a == b

instance Ord a => Ord (IgnoreLocation a) where
    IgnoreLocation (Located _ a) `compare` IgnoreLocation (Located _ b) = a `compare` b

instance Show a => Show (IgnoreLocation a) where
    show (IgnoreLocation (Located _ a)) = Text.Show.show a

instance Pretty SourceRegion where
    pretty (GeneratedRegion fp) = "<generated:" <> pretty fp <> ">"
    pretty (RealSourceRegion (SourceRegion fp start end)) =
        "<" <> pretty fp <> ":" <> pretty start <> "-" <> pretty end <> ">"

instance Pretty RealPosition where
    pretty (Position ln cn) = pretty ln <> ":" <> pretty cn

instance ToJSON s => ToJSON (Located s)

instance ToJSON SourceRegion

instance ToJSON RealSourceRegion

instance ToJSON RealPosition
