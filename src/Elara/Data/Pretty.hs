{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Elara.Data.Pretty (
    escapeChar,
    indentDepth,
    parensIf,
    blockParensIf,
    Pretty (..),
    gpretty,
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
import Elara.Data.Pretty.Styles qualified as Styles
import Elara.Width qualified as Width
import Error.Diagnose (Annotation, defaultStyle)
import GHC.Generics
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

blockParensIf :: Bool -> Doc ann -> Doc ann
blockParensIf True = braces
blockParensIf False = identity

listToText :: forall a. Pretty a => [a] -> Doc AnsiStyle
listToText elements =
    vsep (fmap prettyEntry elements)
  where
    prettyEntry entry = "• " <> align (pretty @a entry)

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
bracedBlock :: Pretty a => [a] -> Doc AnsiStyle
bracedBlock [] = "{}"
bracedBlock b = do
    let open = "{ "
        close = " }"
        shortSeparator = "; "
        longSeparator = "; " <> line
    group (align (encloseSep open close (flatAlt longSeparator shortSeparator) (pretty <$> b)))

class Pretty a where
    pretty :: a -> Doc AnsiStyle
    default pretty :: (Generic a, GPretty (Rep a)) => a -> Doc AnsiStyle
    pretty = gpretty

gpretty :: (Generic a, GPretty (Rep a)) => a -> Doc AnsiStyle
gpretty = sep . gprettyPrec 0 . from

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

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e) => Pretty (a, b, c, d, e) where
    pretty (a, b, c, d, e) = tupled [pretty a, pretty b, pretty c, pretty d, pretty e]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f) => Pretty (a, b, c, d, e, f) where
    pretty (a, b, c, d, e, f) = tupled [pretty a, pretty b, pretty c, pretty d, pretty e, pretty f]

instance {-# OVERLAPPABLE #-} PP.Pretty a => Pretty a where
    pretty = PP.pretty

escapeChar :: IsString s => Char -> s
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

instance {-# OVERLAPPABLE #-} Pretty i => Pretty [i] where
    pretty = align . list . map pretty

instance Pretty i => Pretty (NonEmpty i) where
    pretty = pretty . toList

instance (Pretty a, Pretty b) => Pretty (a, b) where
    pretty (a, b) = tupled [pretty a, pretty b]

instance (Pretty k, Pretty v) => Pretty (Map k v) where
    pretty m = pretty (Map.toList m)

instance Pretty s => Pretty (Set s) where
    pretty = group . encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", " . fmap pretty . toList

-- Generic instances

-- I borrowed a lot of this from https://github.com/tanakh/generic-pretty/blob/master/src/Text/PrettyPrint/Generic.hs
-- so thank you :)

class GPretty f where
    -- | Pretty print a `Generic` value
    gprettyPrec :: Int -> f a -> [Doc AnsiStyle]

instance (GPretty f, GPretty g) => GPretty (f :+: g) where
    gprettyPrec p (L1 x) = gprettyPrec p x
    gprettyPrec p (R1 x) = gprettyPrec p x

instance (GPretty f, GPretty g) => GPretty (f :*: g) where
    gprettyPrec p (x :*: y) = gprettyPrec p x <> gprettyPrec p y

instance Pretty a => GPretty (K1 i a) where
    gprettyPrec _ (K1 x) = [pretty x]

instance GPretty U1 where
    gprettyPrec _ U1 = mempty

instance (GPretty f, Constructor c) => GPretty (C1 c f) where
    gprettyPrec p c@(M1 a)
        | conIsRecord c =
            [con <+> encloseSep (lbrace <> space) (space <> rbrace) (comma <> space) es]
        | null es = [con]
        | p == 0 = [con <+> align (sep es)]
        | otherwise = [parens $ con <+> align (sep es)]
      where
        con = Styles.constructor $ pretty (conName c)
        es = gprettyPrec (p + 1) a

instance (GPretty f, Selector s) => GPretty (S1 s f) where
    gprettyPrec _ s@(M1 a) =
        let name = selName s
         in case name of
                "" -> gprettyPrec 0 a
                _ -> [pretty name <+> Styles.operator "=" <+> sep (gprettyPrec 0 a)]

instance GPretty f => GPretty (D1 c f) where
    gprettyPrec p (M1 x) = gprettyPrec p x
