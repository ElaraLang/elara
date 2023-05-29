module Elara.Data.Pretty.Styles where

import Prettyprinter
import Prettyprinter.Render.Terminal

keyword :: Doc AnsiStyle -> Doc AnsiStyle
keyword = annotate (color Magenta)

moduleNameStyle :: Doc AnsiStyle -> Doc AnsiStyle
moduleNameStyle = annotate (color Blue)

typeName :: Doc AnsiStyle -> Doc AnsiStyle
typeName = annotate (color Yellow)

varName :: Doc AnsiStyle -> Doc AnsiStyle
varName = annotate (color Green)
