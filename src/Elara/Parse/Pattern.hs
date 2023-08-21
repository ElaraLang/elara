module Elara.Parse.Pattern (patParser) where

import Control.Lens (Iso', iso)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Elara.AST.Frontend
import Elara.AST.Generic (Pattern (..), Pattern' (..))
import Elara.AST.Region (Located)
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (liftedBinary)
import Elara.Parse.Literal
import Elara.Parse.Names (conName, varId)
import Elara.Parse.Primitives (HParser, inParens, located, token_, (<??>))
import HeadedMegaparsec (endHead)
import Text.Megaparsec (choice, sepEndBy)

patParser :: HParser FrontendPattern
patParser =
    choice
        [ varPattern
        , zeroArgConstructorPattern
        , literalPattern
        , wildcardPattern
        , inParens apat
        , listPattern
        ]

apat :: HParser FrontendPattern
apat = constructorPattern <|> rpat

rpat :: HParser FrontendPattern
rpat =
    makeExprParser
        patParser
        [ [InfixR cons]
        ]
        <??> "pattern"

unannotatedExpr :: Iso' FrontendPattern (Located FrontendPattern')
unannotatedExpr = iso (\(Pattern (e, _)) -> e) (\x -> Pattern (x, Nothing))

-- TODO: refactor this to allow for more than just cons patterns eg data (:=:) a b = a :=: b; f (x :=: y) = x + y
cons :: HParser (FrontendPattern -> FrontendPattern -> FrontendPattern)
cons = liftedBinary (token_ TokenDoubleColon) (const ConsPattern) unannotatedExpr

locatedPattern :: HParser FrontendPattern' -> HParser FrontendPattern
locatedPattern = ((\x -> Pattern (x, Nothing)) <$>) . located

varPattern :: HParser FrontendPattern
varPattern = locatedPattern (VarPattern <$> located varId)

wildcardPattern :: HParser FrontendPattern
wildcardPattern = locatedPattern (WildcardPattern <$ token_ TokenUnderscore)

listPattern :: HParser FrontendPattern
listPattern = locatedPattern $ do
    token_ TokenLeftBracket
    endHead
    elements <- sepEndBy patParser (token_ TokenComma)
    token_ TokenRightBracket
    pure $ ListPattern elements

-- consPattern :: HParser FrontendPattern
-- consPattern i = locatedPattern $ do
--     (head', tail') <- do
--         head' <- pat' (i + 1)
--         token_ TokenDoubleColon
--         endHead
--         tail' <- pat' (i + 1)
--         pure (head', tail')

--     pure $ ConsPattern head' tail'

-- To prevent ambiguity between space-separated patterns and constructor patterns
zeroArgConstructorPattern :: HParser FrontendPattern
zeroArgConstructorPattern = locatedPattern $ do
    con <- located conName
    pure $ ConstructorPattern con []

constructorPattern :: HParser FrontendPattern
constructorPattern = locatedPattern $ do
    con <- located conName
    endHead
    args <- many patParser
    pure $ ConstructorPattern con args

literalPattern :: HParser FrontendPattern
literalPattern =
    locatedPattern $
        choice
            [ IntegerPattern <$> integerLiteral
            , FloatPattern <$> floatLiteral
            , StringPattern <$> stringLiteral
            , CharPattern <$> charLiteral
            , UnitPattern <$ unitLiteral
            ]
