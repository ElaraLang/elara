import Data.Text
import Parse.Declaration
import Text.Megaparsec

main =
  readFile "source.elr" >>= parseTest (declaration <* eof) . pack