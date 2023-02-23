module Elara.Parse.Combinators (sepBy1') where

import Elara.Parse.Primitives (HParser)
import HeadedMegaparsec (endHead)

-- Safe version of [sepBy1] that backtracks if the parser after the separator fails.
-- Could also be considered a lazy version of [sepBy1]
sepBy1' :: HParser a -> HParser sep -> HParser (NonEmpty a)
sepBy1' p sep = do
    x <- p
    endHead
    (x :|) <$> many (sep *> p)
