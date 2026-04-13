module Elara.Parse.Pattern (patParser) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.List.NonEmpty ((<|))
import Elara.AST.Extensions (ListTuplePatternExtension (..))
import Elara.AST.Name (VarName (NormalVarName))
import Elara.AST.Phases.Frontend
import Elara.AST.Region (Located (..), SourceRegion, enclosingRegion')
import Elara.AST.Types (Pattern (..), Pattern' (..))
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (sepBy1')
import Elara.Parse.Literal
import Elara.Parse.Names (conName, varId)
import Elara.Parse.Primitives (Parser, inParens, located, token_)
import Text.Megaparsec (choice, sepEndBy, try, (<?>))

patternRegion :: FrontendPattern -> SourceRegion
patternRegion (Pattern loc _ _) = loc

atomicPatParser :: Parser FrontendPattern
atomicPatParser =
    choice
        [ try literalPattern
        , varPattern
        , wildcardPattern
        , parensOrTuplePattern
        , unaryConstructorPattern
        , listPattern
        ]

patParser :: Parser FrontendPattern
patParser =
    choice
        [ try constructorPattern
        , try atomicPatParser
        , inParens rpat
        ]

rpat :: Parser FrontendPattern
rpat =
    makeExprParser
        patParser
        [ [InfixR cons]
        ]
        <?> "pattern"

cons :: Parser (FrontendPattern -> FrontendPattern -> FrontendPattern)
cons = do
    token_ TokenDoubleColon
    pure $ \l r ->
        let region = enclosingRegion' (patternRegion l) (patternRegion r)
         in Pattern region Nothing (PExtension (ConsPattern l r))

locatedPattern :: Parser FrontendPattern' -> Parser FrontendPattern
locatedPattern p = (\(Located sr node) -> Pattern sr Nothing node) <$> located p

varPattern :: Parser FrontendPattern
varPattern = locatedPattern (PVar . fmap NormalVarName <$> located varId)

wildcardPattern :: Parser FrontendPattern
wildcardPattern = locatedPattern (PWildcard <$ token_ TokenUnderscore)

listPattern :: Parser FrontendPattern
listPattern = locatedPattern $ do
    token_ TokenLeftBracket
    elements <- sepEndBy patParser (token_ TokenComma)
    token_ TokenRightBracket
    pure $ PExtension (ListPattern elements)

{- | Parse a parenthesized pattern or a tuple pattern in a single pass.
Consumes '(' once, parses the first pattern, then branches on ',' vs ')'.
-}
parensOrTuplePattern :: Parser FrontendPattern
parensOrTuplePattern = do
    Located loc res <- located $ do
        token_ TokenLeftParen
        inner <- rpat
        optional (token_ TokenComma) >>= \case
            Nothing -> do
                token_ TokenRightParen
                pure $ Left inner
            Just () -> do
                rest <- sepBy1' patParser (token_ TokenComma)
                token_ TokenRightParen
                pure $ Right (inner <| rest)
    case res of
        Left p -> pure p
        Right elements -> pure $ Pattern loc Nothing (PExtension (TuplePattern elements))

unaryConstructorPattern :: Parser FrontendPattern
unaryConstructorPattern = locatedPattern $ do
    con <- located conName
    pure $ PCon con []

constructorPattern :: Parser FrontendPattern
constructorPattern = locatedPattern $ do
    con <- located conName
    args <- many atomicPatParser
    pure $ PCon con args

literalPattern :: Parser FrontendPattern
literalPattern =
    locatedPattern $
        choice
            [ PInt <$> integerLiteral
            , PFloat <$> floatLiteral
            , PString <$> stringLiteral
            , PChar <$> charLiteral
            , PUnit <$ unitLiteral
            ]
