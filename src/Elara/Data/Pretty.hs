{-# OPTIONS_GHC -Wno-orphans #-}

module Elara.Data.Pretty (
    escapeChar,
    indentDepth,
    parensIf,
    PrettyPrec (..),
    module Prettyprinter,
) where

import Data.Map qualified as Map (toList)
import Prettyprinter

indentDepth :: Int
indentDepth = 4

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = identity

class (Pretty a) => PrettyPrec a where
    prettyPrec :: Int -> a -> Doc ann
    prettyPrec _ = pretty

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

instance (Pretty k, Pretty v) => Pretty (Map k v) where
    pretty m = pretty (Map.toList m)

instance (Pretty s) => Pretty (Set s) where
    pretty s = "{" <> hsep (punctuate "," (pretty <$> toList s)) <> "}"