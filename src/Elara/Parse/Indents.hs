module Elara.Parse.Indents where

import Elara.AST.Frontend (Expr (..), Expr' (Block), _Expr)

import Control.Lens (view)
import Elara.AST.Region (Located (..), sourceRegion)
import Elara.AST.Region qualified as Region (spanningRegion')
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (sepEndBy1')
import Elara.Parse.Primitives (HParser, IsParser (fromParsec), token_)
import Elara.Parse.Stream (TokenStream (..))
import HeadedMegaparsec
import Text.Megaparsec (MonadParsec (updateParserState), State (stateInput))

indentToken :: HParser ()
indentToken = token_ TokenIndent <|> token_ TokenLeftBrace

dedentToken :: HParser ()
dedentToken = token_ TokenDedent <|> token_ TokenRightBrace

block :: (NonEmpty a -> b) -> (a -> b) -> HParser a -> HParser b
block mergeFunction single exprParser =
    wrapToHead
        ( (single <$> exprParser) <|> do
            indentToken
            endHead
            exprs <- sepEndBy1' exprParser (token_ TokenSemicolon)
            dedentToken
            pure $ mergeFunction exprs
        )

exprBlock :: HParser Expr -> HParser Expr
exprBlock = wrapToHead . block merge identity
  where
    merge :: NonEmpty Expr -> Expr
    merge expressions = case expressions of
        single :| [] -> single
        x :| xs -> do
            let unwrap = view _Expr
            let expressions' = unwrap <$> (x :| xs)
            let region = Region.spanningRegion' (view sourceRegion <$> expressions')
            let asBlock = \case
                    single :| [] -> Expr single
                    o -> Expr (Located region (Block $ Expr <$> o))
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
