module Elara.Parse.Pattern (pattern') where

import Elara.AST.Frontend
import Elara.AST.Generic (Pattern (..), Pattern' (..))
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Literal
import Elara.Parse.Names (typeName, unqualifiedNormalVarName)
import Elara.Parse.Primitives (HParser, inParens, located, token_)
import HeadedMegaparsec (endHead)
import Text.Megaparsec (choice, sepEndBy)

pattern' :: HParser FrontendPattern
pattern' =
    choice
        [ consPattern
        , constructorPattern'
        ]

constructorPattern' :: HParser FrontendPattern
constructorPattern' = choice [zeroArgConstructorPattern, constructorPattern, terminalPattern]

terminalPattern :: HParser FrontendPattern
terminalPattern =
    choice
        [ literalPattern
        , wildcardPattern
        , varPattern
        , listPattern
        , inParens pattern'
        ]

locatedPattern :: HParser FrontendPattern' -> HParser FrontendPattern
locatedPattern = ((\x -> Pattern (x, Nothing)) <$>) . located

varPattern :: HParser FrontendPattern
varPattern = locatedPattern (VarPattern <$> located unqualifiedNormalVarName)

wildcardPattern :: HParser FrontendPattern
wildcardPattern = locatedPattern (WildcardPattern <$ token_ TokenUnderscore)

listPattern :: HParser FrontendPattern
listPattern = locatedPattern $ do
    token_ TokenLeftBracket
    endHead
    elements <- sepEndBy pattern' (token_ TokenComma)
    token_ TokenRightBracket
    pure $ ListPattern elements

consPattern :: HParser FrontendPattern
consPattern = locatedPattern $ do
    (head', tail') <- inParens $ do
        head' <- constructorPattern
        token_ TokenDoubleColon
        endHead
        tail' <- constructorPattern
        pure (head', tail')

    pure $ ConsPattern head' tail'

-- To prevent ambiguity between space-separated patterns and constructor patterns
zeroArgConstructorPattern :: HParser FrontendPattern
zeroArgConstructorPattern = locatedPattern $ do
    con <- located typeName
    pure $ ConstructorPattern con []

constructorPattern :: HParser FrontendPattern
constructorPattern = locatedPattern $ inParens $ do
    con <- located typeName
    endHead
    args <- many terminalPattern
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
