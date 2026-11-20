module Elara.Parse.Combinators (sepBy1', sepEndBy1') where

import Text.Megaparsec (try)

import Elara.Parse.Primitives (Parser)

{- | Safe version of 'Control.Applicative.Combinators.sepBy1' that backtracks if the parser after the separator fails.
Could also be considered a lazy version of [sepBy1]
-}
sepBy1' :: Parser a -> Parser sep -> Parser (NonEmpty a)
sepBy1' p sep = do
    x <- p
    (x :|) <$> many (sep *> p)

{- | Safe version of 'Control.Applicative.Combinators.sepEndBy1' that backtracks if the parser after the separator fails.
Could also be considered a lazy version of 'sepBy1''
-}
sepEndBy1' :: Parser a -> Parser sep -> Parser (NonEmpty a)
sepEndBy1' p sep = do
    x <- p
    (x :|) -- at least one
        <$> many (try (sep *> p)) -- many
        <* optional (try sep) -- optional ending
