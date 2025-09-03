module Elara.Rules where

import Data.Text qualified as Text
import Effectful
import Effectful.FileSystem.IO
import Effectful.State.Static.Local qualified as Local
import Effectful.Writer.Static.Local (runWriter)
import Elara.AST.Name (ModuleName (..))
import Elara.Desugar (getDesugaredModule)
import Elara.Error
import Elara.Lexer.Reader (getLexedFile)
import Elara.Parse (getParsedFileQuery, getParsedModuleQuery)
import Elara.Prim.Rename (primitiveRenameState)
import Elara.Query
import Elara.ReadFile (getInputFiles, runGetFileContentsQuery)
import Elara.Rename (getRenamedModule)
import Elara.Settings (CompilerSettings)
import Elara.Shunt (runGetOpInfoQuery, runGetShuntedModuleQuery)
import Print (showPretty)
import Rock qualified
import System.FilePath (takeFileName)
import System.FilePath.Lens (basename)
import Prelude hiding (withFile)

rules :: Rock.RulesWith CompilerSettings Query
rules compilerSettings key = do
    case key of
        GetCompilerSettings -> pure compilerSettings
        InputFiles -> getInputFiles
        GetFileContents fp -> runGetFileContentsQuery fp
        LexedFile fp -> inject $ getLexedFile fp
        ParsedFile fp -> inject $ getParsedFileQuery fp
        ModulePath (ModuleName ("Main" :| [])) -> do
            pure "source.elr" -- THIS IS BAD
        ModulePath mn@(ModuleName nameParts) -> do
            inputs <- Rock.fetch Elara.Query.InputFiles

            -- search through the inputs for a file with a matching name
            -- TODO: this is very scuffed and makes a LOT of assumptions
            let matchingFiles =
                    [ input
                    | input <- toList inputs
                    , Text.toLower (toText $ takeFileName input) == Text.toLower (last nameParts <> ".elr")
                    ]
            case matchingFiles of
                [fp] -> pure fp
                _ -> error $ "Ambiguous module name: " <> showPretty mn
        ParsedModule mn -> getParsedModuleQuery mn
        DesugaredModule mn -> inject $ getDesugaredModule mn
        RenamedModule mn -> Local.evalState primitiveRenameState $ inject $ getRenamedModule mn
        ShuntedModule mn -> do
            (mod, warnings) <- runWriter $ inject $ runGetShuntedModuleQuery mn
            traverse_ report warnings
            pure mod
        GetOpInfo opName -> inject $ runGetOpInfoQuery opName
