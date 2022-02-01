module Parse.File where

import Parse.Construct
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Indent as Indent

parseElara = Indent.runIndent . Parsec.runParserT file () "<test>"
