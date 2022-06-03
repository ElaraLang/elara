module Main where
import Data.Text
import Text.Megaparsec
import Parse.Module
main =
  readFile "source.elr" >>= parseTest (module' <* eof) . pack