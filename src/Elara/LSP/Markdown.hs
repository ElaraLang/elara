-- | Logic for transforming 'Doc Ann' into Markdown text
module Elara.LSP.Markdown (
    docToMarkdown,
    escapeMarkdown,
) where

import Prettyprinter (Doc, LayoutOptions (..), PageWidth (..), SimpleDocStream (..), layoutPretty)

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as Text

import Elara.Data.Pretty.Styles (Ann)

import Elara.Data.Pretty.Styles qualified as Style

-- | How some text is shown in Markdown
data Format = Plain | Code | Bold
    deriving (Eq, Show)

-- | Some text and formatting
data Piece = Piece
    { pieceFormat :: Format
    , pieceText :: Text
    }
    deriving (Show)

{- | Render a styled 'Doc' as Markdown.
We use the 'Ann' that the compiler annotates documents with for this.

This happens in 3 steps:
1. Split the document into 'Piece's based on the 'Doc' layout ('pieces')
2. Merge and simplify 'Piece's where possible ('mergeNeighbours')
3. Render each 'Piece' ('renderPiece')
-}
docToMarkdown :: Doc Ann -> Text
docToMarkdown =
    foldMap renderPiece
        . mergeNeighbours
        . joinClosingBrackets
        . joinAcrossGlue
        . mergeNeighbours
        . pieces
        . layoutPretty (LayoutOptions Unbounded)

-- | Split a laid out document into 'Piece's of text, with each peace matching the formatting its style describes
pieces :: SimpleDocStream Ann -> [Piece]
pieces = go []
  where
    go :: [Format] -> SimpleDocStream Ann -> [Piece]
    go active = \case
        SFail -> []
        SEmpty -> []
        SChar c rest -> Piece (innermostFormat active) (one c) : go active rest
        SText _ text rest -> Piece (innermostFormat active) text : go active rest
        SLine _ rest -> Piece Plain "\n" : go active rest
        SAnnPush style rest -> go (formatOf style : active) rest
        SAnnPop rest -> go (drop 1 active) rest

    innermostFormat = fromMaybe Plain . viaNonEmpty head . filter (/= Plain)

formatOf :: Ann -> Format
formatOf = \case
    Style.Keyword -> Code
    Style.Punctuation -> Code
    Style.Scalar -> Code
    Style.Builtin -> Code
    Style.ModuleName -> Code
    Style.TypeName -> Code
    Style.VarName -> Code
    Style.Constructor -> Code
    Style.GlobalVar -> Code
    Style.LocalVar -> Code
    Style.Bold -> Bold
    Style.Error -> Bold
    Style.Warning -> Bold

-- | Join pieces that are next to each other and have the same format, ignoring pieces without any text
mergeNeighbours :: [Piece] -> [Piece]
mergeNeighbours = map merge . NE.groupBy ((==) `on` pieceFormat) . filter (not . Text.null . pieceText)
  where
    merge group = Piece (pieceFormat (head group)) (foldMap pieceText group)

{- | Code that is only separated by 'isGlue' text is all one piece of code.

For example @Int -> Int@ is three 'Piece's, separated by spaces, but it is better as one piece.
-}
joinAcrossGlue :: [Piece] -> [Piece]
joinAcrossGlue = \case
    Piece Code before : Piece Plain glue : Piece Code after : rest
        | isGlue glue -> joinAcrossGlue (Piece Code (before <> glue <> after) : rest)
    piece : rest -> piece : joinAcrossGlue rest
    [] -> []

-- | Is some text "glue", i.e. only spaces, dots, commas and brackets, which doesn't look like prose between two bits of code
isGlue :: Text -> Bool
isGlue = Text.all (`elem` (" .,()[]" :: String))

{- | Pull a closing bracket into the code just before it, if the code opened that bracket pair.

= Why this is needed

The pretty-printer only annotates the words, and writes the brackets around them as plain text. So
@Elara.Prim.(+)@ is a module name, plain text @.(@, an operator @+@, and then plain text @)@, as part of
whatever text comes next, e.g. @): Int is not compatible with String@.
'joinAcrossGlue' can join the first three, but the last bracket has plain text after it, so isn't covered.
Without this function we would render @`Elara.Prim.(+`)@, with the closing bracket outside the code.

We only pull brackets in when the code block has more opening brackets than closing brackets (see 'hasUnclosedBracket').
This avoids inadvertently pulling in brackets around normal plain text.

Only when the code has more openers than closers (see 'hasUnclosedBracket'). That means that the bracket must be closing

>>> joinClosingBrackets [Piece Code "Elara.Prim.(+", Piece Plain "): not"]
[Piece {pieceFormat = Code, pieceText = "Elara.Prim.(+)"},Piece {pieceFormat = Plain, pieceText = ": not"}]

>>> joinClosingBrackets [Piece Plain "(", Piece Code "Int", Piece Plain ") is invalid"]
[Piece {pieceFormat = Plain, pieceText = "("},Piece {pieceFormat = Code, pieceText = "Int"},Piece {pieceFormat = Plain, pieceText = ") is invalid"}]

>>> joinClosingBrackets [Piece Code "a (b", Piece Plain "))"]
[Piece {pieceFormat = Code, pieceText = "a (b)"},Piece {pieceFormat = Plain, pieceText = ")"}]

This function can leave an empty 'Piece' behind, which 'mergeNeighbours' will remove.

>>> joinClosingBrackets [Piece Code "Elara.Prim.(+", Piece Plain ")"]
[Piece {pieceFormat = Code, pieceText = "Elara.Prim.(+)"},Piece {pieceFormat = Plain, pieceText = ""}]
-}
joinClosingBrackets :: [Piece] -> [Piece]
joinClosingBrackets = \case
    Piece Code code : Piece Plain text : rest
        | Just (bracket, remainder) <- Text.uncons text
        , isClosingBracket bracket
        , hasUnclosedBracket code ->
            joinClosingBrackets (Piece Code (code <> one bracket) : Piece Plain remainder : rest)
    piece : rest -> piece : joinClosingBrackets rest
    [] -> []

isClosingBracket :: Char -> Bool
isClosingBracket = (`elem` (")]" :: String))

-- | Does this text open a bracket that it doesn't close?
hasUnclosedBracket :: Text -> Bool
hasUnclosedBracket text = openers > closers
  where
    openers = Text.count "(" text + Text.count "[" text
    closers = Text.count ")" text + Text.count "]" text

renderPiece :: Piece -> Text
renderPiece (Piece format text) = case format of
    Plain -> Text.replace "\n" "  \n" (escapeMarkdown text)
    Code -> surround "`" (Text.filter (/= '`') text)
    Bold -> surround "**" text

-- | Put delimiters around some text, handling spaces appropriately
surround :: Text -> Text -> Text
surround delimiter text = case Text.strip text of
    "" -> text
    core -> Text.takeWhile (== ' ') text <> delimiter <> core <> delimiter <> Text.takeWhileEnd (== ' ') text

-- | Escape Markdown formatting characters
escapeMarkdown :: Text -> Text
escapeMarkdown =
    Text.concatMap
        ( \c ->
            if c `elem` ("\\`*_[]<>#|" :: String)
                then toText ['\\', c]
                else one c
        )
