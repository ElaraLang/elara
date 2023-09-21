module Elara.Parse.Indents where

import Control.Lens (view, _1)
import Data.Generics.Wrapped
import Elara.AST.Frontend
import Elara.AST.Generic (Expr (Expr), Expr' (..), NoFieldValue (NoFieldValue))
import Elara.AST.Region (Located (..), sourceRegion)
import Elara.AST.Region qualified as Region (spanningRegion')
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (sepEndBy1')
import Elara.Parse.Primitives (HParser, IsParser (fromParsec), optionallyInParens, token_)
import Elara.Parse.Stream (TokenStream (..))
import HeadedMegaparsec
import Text.Megaparsec (MonadParsec (updateParserState), State (stateInput))

indentToken :: HParser ()
indentToken = token_ TokenIndent <|> token_ TokenLeftBrace

dedentToken :: HParser ()
dedentToken = token_ TokenDedent <|> token_ TokenRightBrace

block :: (NonEmpty a -> b) -> (a -> b) -> HParser a -> HParser b
block mergeFunction single exprParser = wrapToHead (singleBlock <|> wholeBlock)
  where
    singleBlock = single <$> exprParser
    wholeBlock = do
      indentToken
      endHead
      exprs <- sepEndBy1' exprParser (token_ TokenSemicolon)
      dedentToken
      pure $ mergeFunction exprs

exprBlock :: HParser FrontendExpr -> HParser FrontendExpr
exprBlock = optionallyInParens . wrapToHead . block merge identity
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

ignoreFollowingIndents :: (IsParser m) => Int -> m ()
ignoreFollowingIndents n =
  fromParsec
    ( updateParserState
        ( \s ->
            s
              { stateInput =
                  (stateInput s)
                    { tokenStreamIgnoringIndents = tokenStreamIgnoringIndents (stateInput s) + n
                    }
              }
        )
    )

-- | Ignores a certain number of indents
ignoringIndent :: (IsParser m) => Int -> m b -> m b
ignoringIndent n p = do
  ignoreFollowingIndents n
  p
