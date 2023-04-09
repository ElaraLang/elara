module Elara.Parse.Indents where

import Elara.AST.Frontend (Expr (..), Expr' (Block))

import Control.Lens (view)
import Elara.AST.Region (Located (..), sourceRegion)
import Elara.AST.Region qualified as Region (spanningRegion')
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (sepEndBy1')
import Elara.Parse.Primitives (HParser, token_)
import HeadedMegaparsec

block :: HParser Expr -> HParser Expr
block exprParser =
  exprParser <|> do
    token_ TokenLeftBrace
    endHead
    exprs <- sepEndBy1' exprParser (token_ TokenSemicolon)
    token_ TokenRightBrace
    pure $ merge exprs
 where
  merge :: NonEmpty Expr -> Expr
  merge expressions = case expressions of
    single :| [] -> single
    x :| xs -> do
      let unwrap (Expr e) = e
      let expressions' = unwrap <$> (x :| xs)
      let region = Region.spanningRegion' (view sourceRegion <$> expressions')
      let asBlock = \case
            single :| [] -> Expr single
            o -> Expr (Located region (Block $ Expr <$> o))
      asBlock expressions'
