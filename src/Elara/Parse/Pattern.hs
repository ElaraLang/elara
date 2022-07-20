module Elara.Parse.Pattern where

import Elara.Data.Pattern (Pattern (NamedPattern, WildPattern))
import Elara.Parse.Name (alphaVarName)
import Elara.Parse.Primitives (Parser)
import Text.Megaparsec.Char (string)
import Text.Parser.Combinators (choice)

pattern :: Parser Pattern
pattern =
  choice
    [ varPattern,
      wildcardPattern
    ]

varPattern :: Parser Pattern
varPattern = NamedPattern <$> alphaVarName

wildcardPattern :: Parser Pattern
wildcardPattern = WildPattern <$ string "_"