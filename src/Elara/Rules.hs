module Elara.Rules where

import Effectful
import Elara.Lexer.Reader (getLexedFile)
import Elara.Parse (getParsedFileQuery)
import Elara.Query
import Elara.ReadFile (getInputFiles, runGetFileContentsQuery)
import Elara.Settings (CompilerSettings)
import Rock qualified
import Prelude hiding (withFile)

rules :: Rock.RulesWith CompilerSettings Query
rules compilerSettings key = do
    case key of
        InputFiles -> getInputFiles
        GetFileContents fp -> runGetFileContentsQuery fp
        LexedFile fp -> inject $ getLexedFile fp
        ParsedFile fp -> inject $ getParsedFileQuery fp
        GetCompilerSettings -> pure compilerSettings
