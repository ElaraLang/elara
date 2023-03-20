module Elara.Lexer.Utils where

import Text.Read (read)

parseFloat :: Text -> Double
parseFloat = read . toString
