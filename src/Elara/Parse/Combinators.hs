module Elara.Parse.Combinators (sepBy1', sepEndBy1', liftedBinary) where

import Control.Lens (Iso', from, (^.))
import Elara.AST.Region (Located (Located), enclosingRegion', sourceRegion)
import Elara.Parse.Primitives (Parser)
import Text.Megaparsec (try)

-- Safe version of [sepBy1] that backtracks if the parser after the separator fails.
-- Could also be considered a lazy version of [sepBy1]
sepBy1' :: Parser a -> Parser sep -> Parser (NonEmpty a)
sepBy1' p sep = do
    x <- p
    (x :|) <$> many (sep *> p)

-- Safe version of @sepEndBy1@ that backtracks if the parser after the separator fails.
-- Could also be considered a lazy version of [sepBy1]
sepEndBy1' :: Parser a -> Parser sep -> Parser (NonEmpty a)
sepEndBy1' p sep = do
    x <- p
    (x :|) -- at least one
        <$> many (try (sep *> p)) -- many
        <* optional (try sep) -- optional ending

-- Lift a binary operator to work on `Expr` instead of `FrontendExpr`. Probably not the best way to do this, but it works

liftedBinary ::
    Monad m =>
    m t ->
    (t -> a -> a -> a1) ->
    Iso' a (Located a1) ->
    m (a -> a -> a)
liftedBinary op f _Expr = do
    op' <- op
    let create l r =
            let region = enclosingRegion' (l ^. _Expr . sourceRegion) (r ^. _Expr . sourceRegion)
             in Located region (f op' l r) ^. from _Expr
    pure create
