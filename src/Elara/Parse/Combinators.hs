module Elara.Parse.Combinators where

import Elara.Parse.Primitives (Parser)
import Text.Megaparsec (try)

-- Safe version of [sepBy1] that backtracks if the parser after the separator fails.
-- Could also be considered a lazy version of [sepBy1]
sepBy1' :: Parser a -> Parser sep -> Parser (NonEmpty a)
sepBy1' p sep = do
  x <- try p
  (x :|) <$> many (try (sep >> p))