module Elara.Parse.Indents where

import Data.Generics.Wrapped
import Elara.AST.Frontend
import Elara.AST.Generic (Expr (Expr), Expr' (..))
import Elara.AST.Region (Located (..), sourceRegion)
import Elara.AST.Region qualified as Region (spanningRegion')
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (sepEndBy1')
import Elara.Parse.Primitives (Parser, token_)
import Text.Megaparsec (MonadParsec (..))

lineSeparator :: Parser ()
lineSeparator = token_ TokenLineSeparator <|> token_ TokenSemicolon

indentToken :: Parser ()
indentToken = token_ TokenIndent <|> token_ TokenLeftBrace

dedentToken :: Parser ()
dedentToken =
    token_ TokenDedent
        <|> token_ TokenRightBrace

blockExpr :: Parser a -> Parser (NonEmpty a)
blockExpr p = do
    indentToken
    xs <- sepEndBy1' p lineSeparator
    dedentToken
    pure xs

block :: (NonEmpty a -> b) -> (a -> b) -> Parser a -> Parser b
block mergeFunction single exprParser =
    wholeBlock <|> singleBlock
  where
    singleBlock = single <$> exprParser
    wholeBlock = do
        lookAhead indentToken
        exprs <- blockExpr exprParser

        pure $ mergeFunction exprs

exprBlock :: Parser FrontendExpr -> Parser FrontendExpr
exprBlock = block merge identity
  where
    merge :: NonEmpty FrontendExpr -> FrontendExpr
    merge expressions = case expressions of
        single :| [] -> single
        x :| xs -> do
            let expressions' = view _Unwrapped <$> (x :| xs)
            let region = Region.spanningRegion' (view (_1 % sourceRegion) <$> expressions')
            let asBlock = \case
                    single :| [] -> Expr single
                    o -> Expr (Located region (Block $ Expr <$> o), Nothing)
            asBlock expressions'
