{-# LANGUAGE ViewPatterns #-}

module Elara.Lexer.Utils where

import Relude.Unsafe ((!!))
import Text.Read (read)

parseFloat :: Text -> Double
parseFloat = read . toString

translateEscapedChar :: HasCallStack => LByteString -> Char
translateEscapedChar (consumeAmp . decodeUtf8 -> s) = case s of
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


consumeAmp ('\\' : '&' : t) = consumeAmp t -- why is this a thing? who designed these escape sequences man
consumeAmp t = t