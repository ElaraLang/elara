module Elara.Parse.Pattern (pattern') where

import Elara.AST.Frontend (Pattern (..), Pattern' (..))
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Literal
import Elara.Parse.Names (typeName, unqualifiedNormalVarName)
import Elara.Parse.Primitives (HParser, inParens, located, token_)
import HeadedMegaparsec qualified as H (parse, toParsec)
import Text.Megaparsec (choice, sepEndBy)

pattern' :: HParser Pattern
pattern' =
    choice @[]
        [ varPattern
        , wildcardPattern
        , listPattern
        , constructorPattern
        , inParens pattern'
        , literalPattern
        ]

locatedPattern :: HParser Pattern' -> HParser Pattern
locatedPattern = (Pattern <$>) . H.parse . located . H.toParsec

varPattern :: HParser Pattern
varPattern = locatedPattern (VarPattern <$> located unqualifiedNormalVarName)

wildcardPattern :: HParser Pattern
wildcardPattern = locatedPattern (WildcardPattern <$ token_ TokenUnderscore)

listPattern :: HParser Pattern
listPattern = locatedPattern $ do
    token_ TokenLeftBracket
    elements <- sepEndBy pattern' (token_ TokenComma)
    token_ TokenRightBracket
    pure $ ListPattern elements

constructorPattern :: HParser Pattern
constructorPattern = locatedPattern $ do
    con <- located typeName
    args <- many pattern'
    pure $ ConstructorPattern con args

literalPattern :: HParser Pattern
literalPattern =
    locatedPattern $
        choice @[]
            [ IntegerPattern <$> integerLiteral
            , FloatPattern <$> floatLiteral
            , StringPattern <$> stringLiteral
            , CharPattern <$> charLiteral
            ]
