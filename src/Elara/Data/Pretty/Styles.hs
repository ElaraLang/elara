module Elara.Data.Pretty.Styles where

import Data.Text.Prettyprint.Doc.Render.Terminal
import Prettyprinter

keyword :: Doc AnsiStyle -> Doc AnsiStyle
keyword = annotate (color Magenta)

moduleNameStyle :: Doc AnsiStyle -> Doc AnsiStyle
moduleNameStyle = annotate (color Blue)

typeName = annotate (color Yellow)

varName = annotate (color Green)