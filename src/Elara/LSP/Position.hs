-- | Handles mapping between Elara 1-based regions and text, which uses UTF-8, and LSP 0-based ranges which use UTF-16
module Elara.LSP.Position where

import Data.Text qualified as Text
import Language.LSP.Protocol.Types qualified as LSP

import Elara.AST.Region

{- | Turn a 'SourceRegion' into an LSP-compatible 'LSP.Range'.
* 'GeneratedRegion's have no position, so they map to @(0,0)-(0,0)@.
* Positions are clamped to the end of the line/file if they spill over
* Zero-width regions are widened by one code point so the editor has something to underline.
-}
regionToRange ::
    -- | The full contents of the file that the region references, so that we can convert from UTF-8 to UTF-16
    Text ->
    -- | The region to turn into a 'LSP.Range'
    SourceRegion ->
    LSP.Range
regionToRange _ (GeneratedRegion _) = zeroLSPRange
regionToRange text (RealSourceRegion (UnsafeMkSourceRegion _ start end)) =
    LSP.Range (toLspPosition fileLines start) (toLspPosition fileLines end')
  where
    fileLines = Text.splitOn "\n" text

    end'
        | start == end = end{_column = _column end + 1} -- widen an identical start + end to be 1-wide
        | otherwise = end

-- | Convert a 1-based, UTF-8-based Elara position into a 0-based, UTF-16-based LSP position.
toLspPosition ::
    -- | Every line of Text in the file
    [Text] ->
    -- | The exact position (line, column) to convert
    RealPosition ->
    LSP.Position
toLspPosition fileLines (Position ln col) =
    case drop lineIndex fileLines of
        (lineText : _) ->
            LSP.Position (fromIntegral lineIndex) (utf16Length (Text.take (col - 1) lineText))
        [] ->
            -- We've gone past the last line, clamp to the end of the whole file
            LSP.Position (fromIntegral (length fileLines - 1)) (utf16Length (fromMaybe "" (viaNonEmpty last fileLines)))
  where
    lineIndex = max 0 (ln - 1)

-- | The length of some text in UTF-16 code units
utf16Length :: Text -> LSP.UInt
utf16Length = fromIntegral . Text.foldl' (\n c -> n + utf16Width c) (0 :: Int)

-- | The number of UTF-16 code units a character needs.
utf16Width :: Char -> Int
utf16Width c = if ord c > 0xFFFF then 2 else 1

zeroPosition :: LSP.Position
zeroPosition = LSP.Position 0 0

zeroLSPRange :: LSP.Range
zeroLSPRange = LSP.Range zeroPosition zeroPosition

{- | Like 'regionToRange', but looks up the contents of the region's file itself.
If we can't find source text for any part of the region, we return 'zeroLSPRange', i.e. for regions with no paths or regions
that can't be read.
-}
regionToRangeIn ::
    -- | Look up the current contents of a file, if it is known
    (FilePath -> Maybe Text) ->
    SourceRegion ->
    LSP.Range
regionToRangeIn _ (GeneratedRegion _) = zeroLSPRange
regionToRangeIn readFile region@(RealSourceRegion real) =
    maybe zeroLSPRange (`regionToRange` region) (_sourceFile real >>= readFile)

{- | Convert a 0-based, UTF-16-based LSP position into a 1-based, UTF-8-based Elara position.
For a valid position, this is the inverse of 'toLspPosition'

LSP positions can point at places Elara 'RealPosition's can't, so they are normalised as follows:

* A line past the end of the file is clamped to the end of the last line
* A character past the end of a line is clamped to the end of that line
* A character in the middle of a surrogate pair (e.g. inside an emoji) is rounded down to before that character
-}
lspPositionToElara ::
    -- | The full contents of the file the position refers to
    Text ->
    LSP.Position ->
    RealPosition
lspPositionToElara text (LSP.Position lspLine lspChar) =
    case drop lineIndex fileLines of
        (lineText : _) -> Position (lineIndex + 1) (codePointsWithin lineText + 1)
        [] -> Position (length fileLines) (maybe 0 Text.length (viaNonEmpty last fileLines) + 1)
  where
    fileLines = Text.splitOn "\n" text
    lineIndex = fromIntegral lspLine

    -- How many whole UTF-8 code points fit in the first `lspChar` UTF-16 code units of the line
    codePointsWithin = go 0 0 . toString
      where
        go :: Int -> Int -> String -> Int
        go !points !units (c : cs)
            | units + utf16Width c <= fromIntegral lspChar = go (points + 1) (units + utf16Width c) cs
        go points _ _ = points

-- | Does this region contain the given position?
regionContains :: RealPosition -> SourceRegion -> Bool
regionContains _ (GeneratedRegion _) = False
regionContains p (RealSourceRegion (UnsafeMkSourceRegion _ start end))
    | start == end = p == start
    | otherwise = start <= p && p < end
