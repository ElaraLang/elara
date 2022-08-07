{-# LANGUAGE TypeFamilies #-}
module Elara.Parse.Indents where

import Elara.AST.Frontend
import Elara.Data.Located qualified as Located
import Elara.Parse.Primitives
import Text.Megaparsec (try, (<|>))
import Text.Megaparsec.Char.Lexer qualified as L

optionallyIndented :: Show a => Parser a -> Parser LocatedExpr -> Parser (a, LocatedExpr)
optionallyIndented a expression = try (nonIndented a expression) <|> try (indentedBlock a expression)

nonIndented :: Parser a -> Parser b -> Parser (a, b)
nonIndented a expression = do
  a' <- a
  b <- expression
  return (a', b)

indentedBlock :: Show a => Parser a -> Parser LocatedExpr -> Parser (a, LocatedExpr)
indentedBlock ref expression = L.indentBlock scn innerParser
  where
    innerParser = do
      a <- ref
      let merge expressions = case expressions of
            [single] -> return (a, single)
            [] -> error "indentedBlock: empty block"
            x:xs -> do
              let region = Located.spanningRegion (Located.getRegion <$> (x:|xs))
              return (a, Located.located region (Block expressions))

      return $ L.IndentSome Nothing merge expression