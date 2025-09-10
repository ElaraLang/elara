module Elara.Data.Pretty.Styles where

import Prettyprinter
import Prettyprinter.Render.Terminal

keyword :: Doc AnsiStyle -> Doc AnsiStyle
keyword = annotate (color Magenta)

punctuation :: Doc AnsiStyle -> Doc AnsiStyle
punctuation = annotate (bold <> colorDull Green)

label :: Doc AnsiStyle -> Doc AnsiStyle
label = annotate mempty

scalar :: Doc AnsiStyle -> Doc AnsiStyle
scalar = annotate (colorDull Cyan)

builtin :: Doc AnsiStyle -> Doc AnsiStyle
builtin = annotate underlined

operator :: Doc AnsiStyle -> Doc AnsiStyle
operator = punctuation

moduleName :: Doc AnsiStyle -> Doc AnsiStyle
moduleName = annotate (color Blue)

typeName :: Doc AnsiStyle -> Doc AnsiStyle
typeName = annotate (color Yellow)

varName :: Doc AnsiStyle -> Doc AnsiStyle
varName = annotate (color Green)

constructor :: Doc AnsiStyle -> Doc AnsiStyle
constructor = annotate (underlined <> colorDull Yellow)

warning :: Doc AnsiStyle -> Doc AnsiStyle
warning = annotate (bold <> color Yellow)
