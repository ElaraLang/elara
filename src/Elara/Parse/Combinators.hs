module Elara.Parse.Combinators (sepBy1', sepEndBy') where

import Elara.Parse.Primitives (Parser)
import Text.Megaparsec (try)

-- Safe version of [sepBy1] that backtracks if the parser after the separator fails.
-- Could also be considered a lazy version of [sepBy1]
sepBy1' :: Parser a -> Parser sep -> Parser (NonEmpty a)
sepBy1' p sep = do
    x <- try p
    (x :|) <$> many (try (sep *> p))

-- Greedy version of [sepEndBy] that won't backtrack if the parser after the separator fails.
sepEndBy' :: Parser a -> Parser sep -> Parser [a]
sepEndBy' p sep = do
    x <- try p
    xs <- many (try (sep *> p))
    pure (x : xs)