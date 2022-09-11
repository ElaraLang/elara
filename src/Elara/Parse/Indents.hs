

module Elara.Parse.Indents where

import Elara.AST.Frontend
import Elara.Data.Located qualified as Located
import Elara.Parse.Primitives
import Text.Megaparsec (Pos, mkPos, try, unPos)
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
      pure $ L.IndentSome Nothing (merge a) expression
    merge a expressions = case expressions of
      [] -> error "indentedBlock: empty block"
      [single] -> pure (a, single)
      x : xs -> do
        let expressions' = x :| xs
        let region = Located.spanningRegion (Located.getRegion <$> expressions')
        pure (a, Located.located region (Block expressions'))

withIndent :: Pos -> Parser a -> Parser (Pos, a)
withIndent pos parser = do
  new <- L.indentGuard scn GT pos
  res <- parser
  pure (new, res)

withCurrentIndent :: Parser a -> Parser (Pos, a)
withCurrentIndent p = do
  current <- L.indentLevel
  withIndent current p

withIndentOrNormal :: Pos -> Parser a -> Parser (Pos, a)
withIndentOrNormal pos parser = unmodified <|> indented
  where
    unmodified = do
      res <- parser
      pure (pos, res)
    indented = do
      let sub1 = case unPos pos of
            1 -> pos
            x -> mkPos (x - 1)
      new <- L.indentGuard scn GT sub1
      res <- parser
      pure (new, res)


withCurrentIndentOrNormal :: Parser a -> Parser (Pos, a)
withCurrentIndentOrNormal p = do
  current <- L.indentLevel
  withIndentOrNormal current p