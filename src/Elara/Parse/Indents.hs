module Elara.Parse.Indents where

import Elara.AST.Phases.Frontend
import Elara.AST.Region (SourceRegion)
import Elara.AST.Region qualified as Region (spanningRegion')
import Elara.AST.Types (Expr (..), Expr' (..))
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
    exprRegion :: FrontendExpr -> SourceRegion
    exprRegion (Expr loc _ _) = loc

    merge :: NonEmpty FrontendExpr -> FrontendExpr
    merge expressions = case expressions of
        single :| [] -> single
        _ -> do
            let region = Region.spanningRegion' (exprRegion <$> expressions)
            Expr region () (EBlock expressions)
