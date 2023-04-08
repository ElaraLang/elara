{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Elara.Data.Pretty (
    escapeChar,
    indentDepth,
    parensIf,
    Pretty (..),
    module Pretty,
    module Elara.Data.Pretty.Styles,
    module Prettyprinter.Render.Terminal,
    listToText,
) where

import Data.Map qualified as Map (toList)
import Prettyprinter as Pretty hiding (Pretty (..), pretty)
import Prettyprinter qualified as PP
import Elara.Data.Pretty.Styles
import Prettyprinter.Render.Terminal (AnsiStyle)

indentDepth :: Int
indentDepth = 4

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = identity

class Pretty a where
    pretty :: a -> Doc AnsiStyle

instance {-# OVERLAPPABLE #-} (PP.Pretty a) => Pretty a where
    pretty = PP.pretty


-- hack
instance PP.Pretty (Doc AnsiStyle) where
    pretty = unAnnotate


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

listToText :: (Pretty a) => [a] -> Doc AnsiStyle
listToText elements =
    vsep (fmap prettyEntry elements)
  where
    prettyEntry entry = "• " <> align (pretty entry)


instance Pretty i => Pretty [i] where
    pretty =    align . list . map pretty

instance (Pretty a, Pretty b) => Pretty (a, b) where
    pretty (a, b) = tupled [pretty a, pretty b]


instance (Pretty k, Pretty v) => Pretty (Map k v) where
    pretty m = pretty (Map.toList m)

instance (Pretty s) => Pretty (Set s) where
    pretty  = group . encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "
