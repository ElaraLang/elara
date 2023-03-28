{-# LANGUAGE ViewPatterns #-}

module Elara.Lexer.Char where

import Text.Read (read)

translateEscapedChar :: (HasCallStack, ToString s) => s -> Char
translateEscapedChar (consumeAmp . toString -> s) = case s of
    "\\a" -> '\a'
    "\\b" -> '\b'
    "\\f" -> '\f'
    "\\n" -> '\n'
    "\\r" -> '\r'
    "\\t" -> '\t'
    "\\v" -> '\v'
    "\\\"" -> '\"'
    "\\\\" -> '\\'
    "\\'" -> '\''
    oth -> case readMaybe ("'" <> oth <> "'") of -- Not very efficient but easier than handling all the numeric cases
        Just c -> c
        Nothing -> error ("Invalid escape sequence: " <> show oth)

consumeAmp :: String -> String
consumeAmp ('\\' : '&' : t) = consumeAmp t -- why is this a thing? who designed these escape sequences man
consumeAmp t = t