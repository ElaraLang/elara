module Elara.JVM.Emit.Operator where

import Data.Text qualified as T

type OpTranslation = (Text, Text)

translations :: [OpTranslation]
translations =
    map
        (\(a, b) -> (a, b <> "_"))
        [ ("+", "plus")
        , ("-", "minus")
        , ("*", "times")
        , ("/", "divide")
        , ("%", "mod")
        , ("==", "eq")
        , ("=", "equals")
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
translateOperatorName name =
    if name == "<init>"
        then name
        else
            foldr (uncurry T.replace) name translations
