module Elara.Data.Pretty (
    escapeChar,
    indentDepth,
    parensIf,
    PrettyPrec (..),
    stack,
    module Prettyprinter,
) where

import Prettyprinter

indentDepth :: Int
indentDepth = 2

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = identity

class Pretty a => PrettyPrec a where
    prettyPrec :: Int -> a -> Doc ann
    prettyPrec _ = pretty

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

-- | Separate with line breaks
stack :: [Doc a] -> Doc a
stack = align . mconcat . punctuate line