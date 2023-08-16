module Elara.Parse.Pattern (pattern') where

import Elara.AST.Frontend
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Literal
import Elara.Parse.Names (typeName, unqualifiedNormalVarName)
import Elara.Parse.Primitives (HParser, inParens, inParens', located, token_)
import HeadedMegaparsec (endHead)
import HeadedMegaparsec qualified as H (parse, toParsec)
import Text.Megaparsec (choice, sepEndBy)
import Elara.AST.Generic (Pattern(..), Pattern' (..))


pattern' :: HParser FrontendPattern
pattern' =
    choice @[]
        [ varPattern
        , wildcardPattern
        , listPattern
        , consPattern
        , constructorPattern
        , inParens' pattern'
        , literalPattern
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
        head' <- pattern'
        token_ TokenDoubleColon
        endHead
        tail' <- pattern'
        pure (head', tail')

    pure $ ConsPattern head' tail'

constructorPattern :: HParser FrontendPattern
constructorPattern = locatedPattern $ do
    con <- located typeName
    args <- many pattern'
    pure $ ConstructorPattern con args

literalPattern :: HParser FrontendPattern
literalPattern =
    locatedPattern $
        choice @[]
            [ IntegerPattern <$> integerLiteral
            , FloatPattern <$> floatLiteral
            , StringPattern <$> stringLiteral
            , CharPattern <$> charLiteral
            ]
