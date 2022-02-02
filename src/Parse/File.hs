module Parse.File where

import Parse.Construct
import qualified Text.Parsec.Indent as Indent

parseElara = Indent.runIndentParserT file () "<test>"
