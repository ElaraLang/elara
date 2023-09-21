module Elara.Parse.Combinators (sepBy1', sepEndBy1', liftedBinary) where

import Control.Lens (Iso', from, (^.))
import Elara.AST.Region (Located (Located), enclosingRegion', sourceRegion)
import Elara.Parse.Primitives (HParser)
import HeadedMegaparsec (endHead)

-- Safe version of [sepBy1] that backtracks if the parser after the separator fails.
-- Could also be considered a lazy version of [sepBy1]
sepBy1' :: HParser a -> HParser sep -> HParser (NonEmpty a)
sepBy1' p sep = do
  x <- p
  endHead
  (x :|) <$> many (sep *> p)

-- Safe version of @sepEndBy1@ that backtracks if the parser after the separator fails.
-- Could also be considered a lazy version of [sepBy1]
sepEndBy1' :: HParser a -> HParser sep -> HParser (NonEmpty a)
sepEndBy1' p sep = do
  x <- p
  endHead
  (x :|) -- at least one
    <$> many (sep *> p) -- many
    <* optional sep -- optional ending

-- Lift a binary operator to work on `Expr` instead of `FrontendExpr`. Probably not the best way to do this, but it works

liftedBinary ::
  (Monad m) =>
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
