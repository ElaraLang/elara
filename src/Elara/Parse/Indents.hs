module Elara.Parse.Indents where

import Text.Megaparsec (ErrorFancy (ErrorCustom), MonadParsec (..), ParseError (FancyError), anySingle, getOffset, satisfy, skipSome, (<?>))

import Elara.AST.Location
import Elara.AST.Phases.Frontend
import Elara.AST.Region (SourceRegion, sourceRegion, unlocated)
import Elara.AST.Types (Expr (..), Expr' (..), Statement (..), Statement' (..))
import Elara.Lexer.Token (Lexeme, Token (..), isLayoutBreak)
import Elara.Parse.Combinators (sepEndBy1')
import Elara.Parse.Error (ElaraParseError (ExpectedIndentation))
import Elara.Parse.Primitives (Parser, token_)

import Elara.AST.Region qualified as Region (spanningRegion)

lineSeparator :: Parser ()
lineSeparator = token_ TokenLineSeparator <|> token_ TokenSemicolon

-- | Parse and ignore a layout break token (i.e. @<DEDENT>@ or @<LINESEP>@)
layoutBreak :: Parser ()
layoutBreak = void $ satisfy (isLayoutBreak . view unlocated)

-- | The offset and token of the first real token after a sequence of layout breaks.
tokenAfterLayoutBreak :: Parser (Int, Lexeme)
tokenAfterLayoutBreak =
    skipSome layoutBreak
        *> liftA2 (,) getOffset (anySingle <?> "Anything after the layout breaks")

-- | Like 'customFailure', but reports the error at the given offset instead of the current one.
customFailureAt :: Int -> ElaraParseError -> Parser a
customFailureAt offset = parseError . FancyError offset . one . ErrorCustom

-- | A block of one or more @p@s separated by line separators: either an indented block @<INDENT> ... <DEDENT>@ or an explicit @{ ... }@.
blockExpr :: Parser a -> Parser (NonEmpty a)
blockExpr p = indented <|> braced
  where
    indented =
        token_ TokenIndent
            *> sepEndBy1' p lineSeparator
            <* (token_ TokenDedent <|> eof) -- lexer doesn't automatically close dedents in the case of EOF
    braced = token_ TokenLeftBrace *> sepEndBy1' p lineSeparator <* token_ TokenRightBrace

{- | Parses either an indented block of @exprParser@s, or a single one on the same line.

If neither a block nor a body is there because the next line isn't indented further than the enclosing
block, this fails with 'ExpectedIndentation'.
-}
block ::
    -- | The token that the caller just consumed that introduced the block body (e.g. @then@, @with@, @->@), etc
    Token ->
    -- | Process the collected block results, if present
    (NonEmpty a -> b) ->
    -- | Process the collected single result, if present
    (a -> b) ->
    -- | Parser for 1 element of the block
    Parser a ->
    Parser b
block introducer mergeBlock single exprParser =
    wholeBlock <|> singleBlock <|> notIndented
  where
    singleBlock = single <$> exprParser

    wholeBlock = mergeBlock <$> blockExpr exprParser

    notIndented =
        -- no block and no body, so what _is_ next?
        optional (try (lookAhead tokenAfterLayoutBreak)) >>= \case
            Nothing -> empty -- this is an actual EOF, let default error handling kick in
            Just (offset, firstToken) ->
                -- something is present eventually! whatever it is should probably be indented properly...
                customFailureAt offset (ExpectedIndentation introducer (firstToken ^. sourceRegion))

{- | Helper for parsing blocks of statements, merging them based on region and creating a single 'FrontendBlock' wrapping the result.

A single expression statement is returned as the expression itself.
This parses semantically invalid blocks such as blocks ending with @let@ statements, which
are removed later in the desugar phase.
-}
exprBlock ::
    -- | The same as in 'block'
    Token ->
    Parser FrontendStatement ->
    Parser FrontendExpr
exprBlock introducer = block introducer merge single
  where
    stmtRegion :: FrontendStatement -> NodeLoc ExprNode SourceRegion
    stmtRegion (Statement loc _) = loc

    single :: FrontendStatement -> FrontendExpr
    single (Statement _ (SExpr e)) = e
    single stmt = Expr (stmtRegion stmt) () (EExtension (FrontendBlock (stmt :| [])))

    merge :: NonEmpty FrontendStatement -> FrontendExpr
    merge (stmt :| []) = single stmt
    merge statements =
        Expr (Region.spanningRegion (stmtRegion <$> statements)) () (EExtension (FrontendBlock statements))
