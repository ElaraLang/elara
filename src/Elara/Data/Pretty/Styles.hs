module Elara.Data.Pretty.Styles where

import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color, colorDull, underlined)

import Prettyprinter.Render.Terminal qualified as PP

-- | What a document represents, agnostic from how specifically it should be styled.
data Ann
    = Keyword
    | Punctuation
    | Scalar
    | Builtin
    | ModuleName
    | TypeName
    | VarName
    | Constructor
    | GlobalVar
    | LocalVar
    | Warning
    | Error
    | Bold
    deriving (Eq, Show)

-- | How each kind of thing is shown in a terminal
ansiStyle :: Ann -> AnsiStyle
ansiStyle = \case
    Keyword -> color Magenta
    Punctuation -> PP.bold <> colorDull Green
    Scalar -> colorDull Cyan
    Builtin -> underlined
    ModuleName -> color Blue
    TypeName -> color Yellow
    VarName -> color Green
    Constructor -> underlined <> colorDull Yellow
    GlobalVar -> color Green
    LocalVar -> color Blue
    Warning -> PP.bold <> color Yellow
    Error -> PP.bold <> color Red
    Bold -> PP.bold

keyword :: Doc Ann -> Doc Ann
keyword = annotate Keyword

punctuation :: Doc Ann -> Doc Ann
punctuation = annotate Punctuation

label :: Doc Ann -> Doc Ann
label = identity

scalar :: Doc Ann -> Doc Ann
scalar = annotate Scalar

builtin :: Doc Ann -> Doc Ann
builtin = annotate Builtin

operator :: Doc Ann -> Doc Ann
operator = punctuation

moduleName :: Doc Ann -> Doc Ann
moduleName = annotate ModuleName

typeName :: Doc Ann -> Doc Ann
typeName = annotate TypeName

varName :: Doc Ann -> Doc Ann
varName = annotate VarName

constructor :: Doc Ann -> Doc Ann
constructor = annotate Constructor

warning :: Doc Ann -> Doc Ann
warning = annotate Warning

error :: Doc Ann -> Doc Ann
error = annotate Error

bold :: Doc Ann -> Doc Ann
bold = annotate Bold
