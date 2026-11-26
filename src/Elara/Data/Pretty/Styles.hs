module Elara.Data.Pretty.Styles where

import Prettyprinter
import Prettyprinter.Render.Terminal as PP

keywordStyle, punctuationStyle, scalarStyle, builtinStyle, moduleNameStyle, typeNameStyle, varNameStyle, constructorStyle :: AnsiStyle
keywordStyle = color Magenta
punctuationStyle = PP.bold <> colorDull Green
scalarStyle = colorDull Cyan
builtinStyle = underlined
moduleNameStyle = color Blue
typeNameStyle = color Yellow
varNameStyle = color Green
constructorStyle = underlined <> colorDull Yellow

warningStyle, errorStyle, boldStyle :: AnsiStyle
warningStyle = PP.bold <> color Yellow
errorStyle = PP.bold <> color Red
boldStyle = PP.bold

keyword :: Doc AnsiStyle -> Doc AnsiStyle
keyword = annotate keywordStyle

punctuation :: Doc AnsiStyle -> Doc AnsiStyle
punctuation = annotate punctuationStyle

label :: Doc AnsiStyle -> Doc AnsiStyle
label = annotate mempty

scalar :: Doc AnsiStyle -> Doc AnsiStyle
scalar = annotate scalarStyle

builtin :: Doc AnsiStyle -> Doc AnsiStyle
builtin = annotate builtinStyle

operator :: Doc AnsiStyle -> Doc AnsiStyle
operator = punctuation

moduleName :: Doc AnsiStyle -> Doc AnsiStyle
moduleName = annotate moduleNameStyle

typeName :: Doc AnsiStyle -> Doc AnsiStyle
typeName = annotate typeNameStyle

varName :: Doc AnsiStyle -> Doc AnsiStyle
varName = annotate varNameStyle

constructor :: Doc AnsiStyle -> Doc AnsiStyle
constructor = annotate constructorStyle

warning :: Doc AnsiStyle -> Doc AnsiStyle
warning = annotate warningStyle

error :: Doc AnsiStyle -> Doc AnsiStyle
error = annotate errorStyle

bold :: Doc AnsiStyle -> Doc AnsiStyle
bold = annotate boldStyle
