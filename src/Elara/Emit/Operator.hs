module Elara.Emit.Operator where

import Data.Text qualified as T

type OpTranslation = (Text, Text)

translations :: [OpTranslation]
translations =
    [ ("+", "plus")
    , ("-", "minus")
    , ("*", "times")
    , ("/", "divide")
    , ("%", "mod")
    , ("==", "eq")
    , ("!=", "neq")
    , ("|>", "pipe")
    , ("<", "lt")
    , (">", "gt")
    , ("<=", "lte")
    , (">=", "gte")
    , ("&", "bitand")
    , ("&&", "and")
    , ("|", "bitor")
    , ("^", "bitxor")
    , ("||", "or")
    , ("!", "not")
    , (">>", "then")
    , (">>=", "bind")
    ]

translateOperatorName :: Text -> Text
translateOperatorName name = foldr (uncurry T.replace) name translations
