module Elara.Parse.Indents where

import Elara.AST.Frontend
import Elara.Data.Located qualified as Located
import Elara.Parse.Primitives
import Text.Megaparsec (try)
import Text.Megaparsec.Char.Lexer qualified as L

optionallyIndented :: Parser a -> Parser LocatedExpr -> Parser (a, LocatedExpr)
optionallyIndented a expression = try (indentedBlock a expression) <|> try (nonIndented a expression)

nonIndented :: Parser a -> Parser b -> Parser (a, b)
nonIndented a expression = do
  a' <- a
  b <- expression
  pure (a', b)

indentedBlock :: Parser a -> Parser LocatedExpr -> Parser (a, LocatedExpr)
indentedBlock ref expression = L.indentBlock scn innerParser
  where
    innerParser = do
      a <- ref
      let merge expressions = case expressions of
            [] -> error "indentedBlock: empty block"
            [single] -> pure (a, single)
            x : xs -> do
              let expressions' = x :| xs
              let region = Located.spanningRegion (Located.getRegion <$> expressions')
              pure (a, Located.located region (Block expressions'))

      pure $ L.IndentSome Nothing merge expression