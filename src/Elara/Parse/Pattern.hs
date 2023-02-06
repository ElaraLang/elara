module Elara.Parse.Pattern (pattern') where

import Elara.AST.Frontend (Pattern (..), Pattern' (..))
import Elara.Parse.Names (alphaVarName, typeName)
import Elara.Parse.Primitives (Parser, inParens, lexeme, located, sc, symbol)
import Text.Parser.Combinators (choice, sepEndBy)

pattern' :: Parser Pattern
pattern' =
    choice
        [ varPattern
        , wildcardPattern
        , listPattern
        , constructorPattern
        , inParens pattern'
        ]

locatedPattern = (Pattern <$>) . located

varPattern :: Parser Pattern
varPattern = locatedPattern (NamedPattern <$> alphaVarName)

wildcardPattern :: Parser Pattern
wildcardPattern = locatedPattern (WildcardPattern <$ symbol "_")

listPattern :: Parser Pattern
listPattern = locatedPattern $ do
    symbol "["
    elements <- lexeme (sepEndBy pattern' (symbol ","))
    symbol "]"
    pure $ ListPattern elements

constructorPattern :: Parser Pattern
constructorPattern = locatedPattern $ do
    con <- lexeme typeName
    args <- lexeme (sepEndBy pattern' sc)
    pure $ ConstructorPattern con args