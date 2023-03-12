module Elara.Parse.Indents where

import Elara.AST.Frontend (Expr (..), Expr' (Block))

import Control.Lens (mapped, over, view, (^.))
import Elara.AST.Region (Located (..), sourceRegion)
import Elara.AST.Region qualified as Region (spanningRegion')
import Elara.Parse.Combinators (sepBy1')
import Elara.Parse.Primitives (HParser, Parser, scn, skipNewlines)
import HeadedMegaparsec qualified as H
import Text.Megaparsec (MonadParsec (try), Pos, mkPos, pos1, unPos)
import Text.Megaparsec.Char.Lexer qualified as L

optionallyIndented :: HParser a -> HParser Expr -> HParser (a, Expr)
optionallyIndented a expression = indentedBlock a expression <|> nonIndented a expression

optionallyIndented' :: HParser a -> HParser Expr -> HParser Expr
optionallyIndented' = (fmap snd .) . optionallyIndented

nonIndented :: HParser a -> HParser b -> HParser (a, b)
nonIndented a expression = do
    a' <- a
    b <- expression
    pure (a', b)

indentedBlock :: HParser a -> HParser Expr -> HParser (a, Expr)
indentedBlock ref expression = H.parse (L.indentBlock scn innerParser)
  where
    innerParser = H.toParsec $ do
        a <- ref
        H.endHead
        pure $ L.IndentSome Nothing (merge a) (H.toParsec expression)
    merge a expressions = case expressions of
        [] -> error "indentedBlock: empty block. shouldn't happen"
        [single] -> pure (a, single)
        x : xs -> do
            let unwrap (Expr e) = e
            let expressions' = unwrap <$> (x :| xs)
            let region = Region.spanningRegion' (over mapped (^. sourceRegion) expressions')
            let asBlock = \case
                    single :| [] -> Expr single
                    o -> Expr (Located region (Block $ Expr <$> o))
            pure (a, asBlock expressions')

withIndent :: Pos -> HParser a -> HParser (Pos, a)
withIndent pos parser = H.parse $ do
    new <- L.indentGuard scn GT pos
    res <- H.toParsec parser
    pure (new, res)

withCurrentIndent :: HParser a -> HParser (Pos, a)
withCurrentIndent p = do
    current <- H.parse L.indentLevel
    withIndent current p

{- | Parses with the given parser, either starting at the given position, or indented after the given position.
     Either way, it returns the position where the parser starts, and the parsed expression
-}
withIndentOrNormal :: Pos -> HParser a -> HParser (Pos, a)
withIndentOrNormal pos parser = unmodified <|> indented
  where
    unmodified = do
        res <- parser
        pure (pos, res)
    indented = H.parse $ sub1Indent pos (H.toParsec parser)

sub1Indent :: Pos -> Parser a -> Parser (Pos, a)
sub1Indent pos parser = case unPos pos of
    1 -> try ((pos,) <$> L.nonIndented scn parser) <|> (L.indentGuard scn GT pos1 >>= \new -> (new,) <$> parser)
    x -> L.indentGuard scn GT (mkPos (x - 1)) >>= \new -> (new,) <$> parser

sub1 :: Pos -> Pos
sub1 pos = case unPos pos of
    1 -> pos
    x -> mkPos (x - 1)

-- | Parses a block, starting at the given position. The given parser should parse a single expression, which will be merged into a block
blockAt :: Pos -> HParser Expr -> HParser (Pos, Expr)
blockAt pos parser = do
    skipNewlines
    exprs <- fmap snd <$> sepBy1' (withIndent (sub1 pos) parser) (H.parse scn)
    pure (pos, merge exprs)
  where
    merge :: NonEmpty Expr -> Expr
    merge expressions = case expressions of
        single :| [] -> single
        x :| xs -> do
            let unwrap (Expr e) = e
            let expressions' = unwrap <$> (x :| xs)
            let region = Region.spanningRegion' (over mapped (view sourceRegion) expressions')
            let asBlock = \case
                    single :| [] -> Expr single
                    o -> Expr (Located region (Block $ Expr <$> o))
            asBlock expressions'

withCurrentIndentOrNormal :: HParser a -> HParser (Pos, a)
withCurrentIndentOrNormal p = do
    current <- H.parse L.indentLevel
    withIndentOrNormal current p
