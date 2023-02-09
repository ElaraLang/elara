module Elara.Parse.Indents where

import Elara.AST.Frontend (Expr (..), Expr' (Block))

import Elara.AST.Region (Located (..))
import Elara.AST.Region qualified as Region (getLocation, spanningRegion)
import Elara.Parse.Combinators (sepBy1')
import Elara.Parse.Primitives (Parser, scn)
import Text.Megaparsec (Pos, mkPos, try, unPos)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer qualified as L

optionallyIndented :: Parser a -> Parser Expr -> Parser (a, Expr)
optionallyIndented a expression = try (indentedBlock a expression) <|> try (nonIndented a expression)

optionallyIndented' :: Parser a -> Parser Expr -> Parser Expr
optionallyIndented' = (fmap snd .) . optionallyIndented

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

{- | Parses with the given parser, either starting at the given position, or indented after the given position.
     Either way, it returns the position where the parser starts, and the parsed expression
-}
withIndentOrNormal :: Pos -> Parser a -> Parser (Pos, a)
withIndentOrNormal pos parser = try unmodified <|> indented
  where
    unmodified = do
        res <- parser
        pure (pos, res)
    indented = do
        new <- L.indentGuard scn GT (sub1 pos)
        res <- parser
        pure (new, res)

sub1 :: Pos -> Pos
sub1 pos = case unPos pos of
    1 -> pos
    x -> mkPos (x - 1)

-- | Parses a block, starting at the given position. The given parser should parse a single expression, which will be merged into a block
blockAt :: Pos -> Parser Expr -> Parser (Pos, Expr)
blockAt pos parser = do
    _ <- many newline
    exprs <- fmap snd <$> sepBy1' (withIndent (sub1 pos) parser) scn
    pure (pos, merge exprs)
  where
    merge :: NonEmpty Expr -> Expr
    merge expressions = case expressions of
        single :| [] -> single
        x :| xs -> do
            let unwrap (Expr e) = e
            let expressions' = unwrap <$> (x :| xs)
            let region = Region.spanningRegion (Region.getLocation <$> expressions')
            let asBlock = \case
                    single :| [] -> Expr single
                    o -> Expr (Located region (Block $ Expr <$> o))
            asBlock expressions'

withCurrentIndentOrNormal :: Parser a -> Parser (Pos, a)
withCurrentIndentOrNormal p = do
    current <- L.indentLevel
    withIndentOrNormal current p
