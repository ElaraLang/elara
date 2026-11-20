module Elara.Parse.Indents where

import Text.Megaparsec (MonadParsec (..))

import Elara.AST.Location
import Elara.AST.Phases.Frontend
import Elara.AST.Region (SourceRegion)
import Elara.AST.Types (Expr (..), Expr' (..))
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (sepEndBy1')
import Elara.Parse.Primitives (Parser, token_)

import Elara.AST.Region qualified as Region (spanningRegion)

lineSeparator :: Parser ()
lineSeparator = token_ TokenLineSeparator <|> token_ TokenSemicolon

indentToken :: Parser ()
indentToken = token_ TokenIndent <|> token_ TokenLeftBrace

dedentToken :: Parser ()
dedentToken =
    token_ TokenDedent
        <|> token_ TokenRightBrace

-- | A block of one or more @p@s separated by line separators: either an indented block @<INDENT> ... <DEDENT>@ or an explicit @{ ... }@.
blockExpr :: Parser a -> Parser (NonEmpty a)
blockExpr p = indented <|> braced
  where
    indented =
        token_ TokenIndent
            *> sepEndBy1' p lineSeparator
            <* (token_ TokenDedent <|> eof) -- lexer doesn't automatically close dedents in the case of EOF
    braced = token_ TokenLeftBrace *> sepEndBy1' p lineSeparator <* token_ TokenRightBrace

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
    exprRegion :: FrontendExpr -> NodeLoc ExprNode SourceRegion
    exprRegion (Expr loc _ _) = loc

    merge :: NonEmpty FrontendExpr -> FrontendExpr
    merge expressions = case expressions of
        single :| [] -> single
        _ -> do
            let region = Region.spanningRegion (exprRegion <$> expressions)
            Expr region () (EBlock expressions)
