{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Elara.Data.Pretty (
    escapeChar,
    indentDepth,
    parensIf,
    Pretty (..),
    module Pretty,
    module Prettyprinter.Render.Terminal,
    listToText,
    bracedBlock,
    renderStrict,
    prettyToText,
    prettyToUnannotatedText,
)
where

import Data.Map qualified as Map (toList)

import Elara.Width qualified as Width
import Error.Diagnose (Annotation, defaultStyle)
import Prettyprinter as Pretty hiding (Pretty (..), pretty)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as Pretty.Terminal (renderStrict)
import Prettyprinter.Render.Text qualified as Pretty.Text
import Prelude hiding (group)

indentDepth :: Int
indentDepth = 4

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = identity

listToText :: (Pretty a) => [a] -> Doc AnsiStyle
listToText elements =
    vsep (fmap prettyEntry elements)
  where
    prettyEntry entry = "• " <> align (pretty entry)

prettyToText :: Pretty a => a -> Text
prettyToText = renderStrict True Width.defaultWidth

prettyToUnannotatedText :: Pretty a => a -> Text
prettyToUnannotatedText = renderStrictUnannotated Width.defaultWidth

renderStrict ::
    Pretty a =>
    -- | `True` enable syntax highlighting
    Bool ->
    -- | Available columns
    Int ->
    a ->
    Text
renderStrict highlight columns =
    render . Pretty.layoutSmart (layoutOptions columns) . pretty
  where
    render =
        if highlight
            then Pretty.Terminal.renderStrict
            else Pretty.Text.renderStrict

renderStrictUnannotated ::
    Pretty a =>
    -- | Available columns
    Int ->
    a ->
    Text
renderStrictUnannotated =
    renderStrict False

layoutOptions ::
    -- | Available columns
    Int ->
    LayoutOptions
layoutOptions columns =
    LayoutOptions{layoutPageWidth = AvailablePerLine columns 1}

{- | A haskell-style braced block, which can split over multiple lines.
 >>> bracedBlock ["foo", "bar"]
 { foo; bar }
-}
bracedBlock :: (Pretty a) => [a] -> Doc AnsiStyle
bracedBlock [] = "{}"
bracedBlock b = do
    let open = "{ "
        close = " }"
        shortSeparator = "; "
        longSeparator = "; " <> line
    group (align (encloseSep open close (flatAlt longSeparator shortSeparator) (pretty <$> b)))

class Pretty a where
    pretty :: a -> Doc AnsiStyle
    default pretty :: (Show a) => a -> Doc AnsiStyle
    pretty = pretty @Text . show

instance Pretty (Doc (Annotation AnsiStyle)) where
    pretty = pretty . reAnnotate defaultStyle

instance Pretty (Doc AnsiStyle) where
    pretty = identity

instance Pretty () where
    pretty = mempty

instance Pretty Bool where
    pretty = PP.pretty

instance Pretty Text where
    pretty = PP.pretty

instance Pretty Int where
    pretty = PP.pretty

instance Pretty Integer where
    pretty = PP.pretty

instance Pretty Double where
    pretty = PP.pretty

instance Pretty Float where
    pretty = PP.pretty

instance Pretty Char where
    pretty = PP.pretty

instance Pretty a => Pretty (Maybe a) where
    pretty = maybe mempty pretty

instance (Pretty a, Pretty b) => Pretty (Either a b) where
    pretty = either pretty pretty

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
    pretty (a, b, c) = tupled [pretty a, pretty b, pretty c]

instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
    pretty (a, b, c, d) = tupled [pretty a, pretty b, pretty c, pretty d]

-- instance {-# OVERLAPPABLE #-} (PP.Pretty a) => Pretty a where
--     pretty = PP.pretty

escapeChar :: (IsString s) => Char -> s
escapeChar c = case c of
    '\a' -> "\\a"
    '\b' -> "\\b"
    '\f' -> "\\f"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    '\v' -> "\\v"
    '\\' -> "\\\\"
    '\'' -> "\\'"
    '"' -> "\\\""
    _ -> fromString [c]

instance {-# INCOHERENT #-} Pretty String where
    pretty = pretty . toText

instance {-# OVERLAPPABLE #-} (Pretty i) => Pretty [i] where
    pretty = align . list . map pretty

instance (Pretty i) => Pretty (NonEmpty i) where
    pretty = pretty . toList

instance (Pretty a, Pretty b) => Pretty (a, b) where
    pretty (a, b) = tupled [pretty a, pretty b]

instance (Pretty k, Pretty v) => Pretty (Map k v) where
    pretty m = pretty (Map.toList m)

instance (Pretty s) => Pretty (Set s) where
    pretty = group . encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", " . fmap pretty . toList
