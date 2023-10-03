module Elara.Parse.Indents where

import Control.Lens (view, _1)
import Data.Generics.Wrapped
import Elara.AST.Frontend
import Elara.AST.Generic (Expr (Expr), Expr' (..))
import Elara.AST.Region (Located (..), sourceRegion)
import Elara.AST.Region qualified as Region (spanningRegion')
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (sepEndBy1')
import Elara.Parse.Primitives (Parser, token_)

import Text.Megaparsec (try)

indentToken :: Parser ()
indentToken = token_ TokenIndent <|> token_ TokenLeftBrace

dedentToken :: Parser ()
dedentToken = token_ TokenDedent <|> token_ TokenRightBrace

block :: (NonEmpty a -> b) -> (a -> b) -> Parser a -> Parser b
block mergeFunction single exprParser = try singleBlock <|> wholeBlock
  where
    singleBlock = single <$> exprParser
    wholeBlock = do
        indentToken
        exprs <- sepEndBy1' exprParser (token_ TokenSemicolon)
        dedentToken
        pure $ mergeFunction exprs

exprBlock :: Parser FrontendExpr -> Parser FrontendExpr
exprBlock = block merge identity
  where
    merge :: NonEmpty FrontendExpr -> FrontendExpr
    merge expressions = case expressions of
        single :| [] -> single
        x :| xs -> do
            let expressions' = view _Unwrapped <$> (x :| xs)
            let region = Region.spanningRegion' (view (_1 . sourceRegion) <$> expressions')
            let asBlock = \case
                    single :| [] -> Expr single
                    o -> Expr (Located region (Block $ Expr <$> o), Nothing)
            asBlock expressions'
