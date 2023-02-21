module Elara.Parse.Pattern (pattern') where

import Elara.AST.Frontend (Pattern (..), Pattern' (..))
import Elara.Parse.Names (alphaVarName, typeName)
import Elara.Parse.Primitives (HParser, Parser, inParens, lexeme, located, sc, symbol)
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
        ]

locatedPattern :: HParser Pattern' -> HParser Pattern
locatedPattern = (Pattern <$>) . H.parse . located . H.toParsec

varPattern :: HParser Pattern
varPattern = locatedPattern (NamedPattern <$> alphaVarName)

wildcardPattern :: HParser Pattern
wildcardPattern = locatedPattern (WildcardPattern <$ symbol "_")

listPattern :: HParser Pattern
listPattern = locatedPattern $ do
    symbol "["
    elements <- lexeme (sepEndBy pattern' (symbol ","))
    symbol "]"
    pure $ ListPattern elements

constructorPattern :: HParser Pattern
constructorPattern = locatedPattern $ do
    con <- lexeme typeName
    args <- lexeme (sepEndBy pattern' sc)
    pure $ ConstructorPattern con args