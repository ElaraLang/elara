module Elara.Parse.Indents where

import Elara.AST.Frontend
import Elara.Data.Located qualified as Located
import Elara.Parse.Expression
import Elara.Parse.Primitives
import Text.Megaparsec (try, (<|>))
import Text.Megaparsec.Char.Lexer qualified as L

optionallyIndented :: Parser a -> Parser (a, LocatedExpr)
optionallyIndented a = try nonIndented <|> try (indentedBlock a)
  where
    nonIndented = do
      a' <- a
      b <- expression
      return (a', b)

indentedBlock :: Parser a -> Parser (a, LocatedExpr)
indentedBlock ref = L.nonIndented scn $
  L.indentBlock scn $ do
    a <- ref
    return $
      L.IndentSome
        Nothing
        ( \x -> do
            let region = Located.spanningRegion (Located.getRegion <$> x)
            return (a, Located.located region (Block x))
        )
        expression