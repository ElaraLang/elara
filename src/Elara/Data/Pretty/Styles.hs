module Elara.Data.Pretty.Styles where

import Prettyprinter
import Prettyprinter.Render.Terminal

keyword :: Doc AnsiStyle -> Doc AnsiStyle
keyword = annotate (color Magenta)

punctuation :: Doc AnsiStyle -> Doc AnsiStyle
punctuation = annotate (bold <> colorDull Green)

label :: Doc AnsiStyle -> Doc AnsiStyle
label = annotate mempty

builtin :: Doc AnsiStyle -> Doc AnsiStyle
builtin = annotate (underlined)

operator :: Doc AnsiStyle -> Doc AnsiStyle
operator = punctuation

moduleNameStyle :: Doc AnsiStyle -> Doc AnsiStyle
moduleNameStyle = annotate (color Blue)

typeName :: Doc AnsiStyle -> Doc AnsiStyle
typeName = annotate (color Yellow)

varName :: Doc AnsiStyle -> Doc AnsiStyle
varName = annotate (color Green)
