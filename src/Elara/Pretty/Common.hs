module Elara.Pretty.Common (
    prettyCtorsInline,
    prettyMatchAlts,
    prettyMatchAlt,
)
where

import Prettyprinter (Doc, flatAlt, group, hsep, indent, line, vsep, (<+>))

import Elara.Data.Pretty.Styles (Ann)
import Prelude hiding (group)

prettyCtorsInline :: [Doc Ann] -> Doc Ann
prettyCtorsInline = \case
    [] -> mempty
    [single] -> single
    (first : rest) ->
        group
            ( flatAlt
                (first <> mconcat (map (\d -> line <> "| " <> d) rest))
                (first <+> hsep (map ("| " <>) rest))
            )

prettyMatchAlts :: Int -> [Doc Ann] -> Doc Ann
prettyMatchAlts n = \case
    [] -> mempty
    alts -> line <> indent n (vsep alts)

prettyMatchAlt :: Doc Ann -> Doc Ann -> Doc Ann
prettyMatchAlt pat body = pat <+> "->" <+> body
