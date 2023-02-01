module Elara.Parse.Indents where

import Elara.AST.Frontend (Expr (..), Expr' (Block))

import Elara.AST.Region (Located (..))
import Elara.AST.Region qualified as Region (getLocation, spanningRegion)
import Elara.Parse.Primitives (Parser, scn)
import Text.Megaparsec (Pos, mkPos, try, unPos)
import Text.Megaparsec.Char.Lexer qualified as L

optionallyIndented :: Parser a -> Parser Expr -> Parser (a, Expr)
optionallyIndented a expression = try (indentedBlock a expression) <|> try (nonIndented a expression)

nonIndented :: Parser a -> Parser b -> Parser (a, b)
nonIndented a expression = do
    a' <- a
    b <- expression
    pure (a', b)

indentedBlock :: Parser a -> Parser Expr -> Parser (a, Expr)
indentedBlock ref expression = L.indentBlock scn innerParser
  where
    innerParser = do
        a <- ref
        pure $ L.IndentSome Nothing (merge a) expression
    merge a expressions = case expressions of
        [] -> error "indentedBlock: empty block"
        [single] -> pure (a, single)
        x : xs -> do
            let unwrap (Expr e) = e
            let expressions' = unwrap <$> (x :| xs)
            let region = Region.spanningRegion (Region.getLocation <$> expressions')
            let asBlock = \case
                    single :| [] -> Expr single
                    o -> Expr (Located region (Block $ Expr <$> o))
            pure (a, asBlock expressions')

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
