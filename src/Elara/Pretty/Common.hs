module Elara.Pretty.Common (
    prettyCtorsInline,
    prettyMatchAlts,
    prettyMatchAlt,
)
where

import Prettyprinter (Doc, flatAlt, group, hsep, indent, line, vsep, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prelude hiding (group)

prettyCtorsInline :: [Doc AnsiStyle] -> Doc AnsiStyle
prettyCtorsInline = \case
    [] -> mempty
    [single] -> single
    (first : rest) ->
        group
            ( flatAlt
                (first <> mconcat (map (\d -> line <> "| " <> d) rest))
                (first <+> hsep (map ("| " <>) rest))
            )

prettyMatchAlts :: Int -> [Doc AnsiStyle] -> Doc AnsiStyle
prettyMatchAlts n = \case
    [] -> mempty
    alts -> line <> indent n (vsep alts)

prettyMatchAlt :: Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
prettyMatchAlt pat body = pat <+> "->" <+> body
