module Elara.Parse.Pattern (patParser) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Elara.AST.Frontend
import Elara.AST.Generic (Pattern (..), Pattern' (..))
import Elara.AST.Region (Located)
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (liftedBinary)
import Elara.Parse.Literal
import Elara.Parse.Names (conName, varId)
import Elara.Parse.Primitives (Parser, inParens, located, token_)
import Text.Megaparsec (choice, sepEndBy, try, (<?>))

patParser :: Parser FrontendPattern
patParser =
    choice
        [ try literalPattern
        , inParens rpat
        , varPattern
        , constructorPattern
        , wildcardPattern
        , listPattern
        ]

rpat :: Parser FrontendPattern
rpat =
    makeExprParser
        patParser
        [ [InfixR cons]
        ]
        <?> "pattern"

unannotatedPattern :: Iso' FrontendPattern (Located FrontendPattern')
unannotatedPattern = iso (\(Pattern (e, _)) -> e) (\x -> Pattern (x, Nothing))

-- TODO: refactor this to allow for more than just cons patterns eg data (:=:) a b = a :=: b; f (x :=: y) = x + y
cons :: Parser (FrontendPattern -> FrontendPattern -> FrontendPattern)
cons = liftedBinary (token_ TokenDoubleColon) (const (curry ConsPattern)) unannotatedPattern

locatedPattern :: Parser FrontendPattern' -> Parser FrontendPattern
locatedPattern = ((\x -> Pattern (x, Nothing)) <$>) . located

varPattern :: Parser FrontendPattern
varPattern = locatedPattern (VarPattern <$> located varId)

wildcardPattern :: Parser FrontendPattern
wildcardPattern = locatedPattern (WildcardPattern <$ token_ TokenUnderscore)

listPattern :: Parser FrontendPattern
listPattern = locatedPattern $ do
    token_ TokenLeftBracket

    elements <- sepEndBy patParser (token_ TokenComma)
    token_ TokenRightBracket
    pure $ ListPattern elements

-- consPattern :: Parser FrontendPattern
-- consPattern i = locatedPattern $ do
--     (head', tail') <- do
--         head' <- pat' (i + 1)
--         token_ TokenDoubleColon
--         endHead
--         tail' <- pat' (i + 1)
--         pure (head', tail')

--     pure $ ConsPattern head' tail'

constructorPattern :: Parser FrontendPattern
constructorPattern = locatedPattern $ do
    con <- located conName

    args <- many patParser
    pure $ ConstructorPattern con args

literalPattern :: Parser FrontendPattern
literalPattern =
    locatedPattern $
        choice
            [ IntegerPattern <$> integerLiteral
            , FloatPattern <$> floatLiteral
            , StringPattern <$> stringLiteral
            , CharPattern <$> charLiteral
            , UnitPattern <$ unitLiteral
            ]
