module Elara.Rules where

import Data.Text qualified as Text
import Effectful
import Effectful.State.Static.Local qualified as Local
import Effectful.Writer.Static.Local (runWriter)
import Elara.AST.Name (ModuleName (..))
import Elara.Core.LiftClosures (runGetClosureLiftedModuleQuery)
import Elara.CoreToCore (runGetANFCoreModuleQuery, runGetFinalisedCoreModuleQuery, runGetOptimisedCoreModuleQuery)
import Elara.Desugar (getDesugaredModule)
import Elara.Error
import Elara.Lexer.Reader (getLexedFile)
import Elara.Parse (getParsedFileQuery, getParsedModuleQuery)
import Elara.Prim.Rename (primitiveRenameState)
import Elara.Query
import Elara.ReadFile (getInputFiles, runGetFileContentsQuery)
import Elara.Rename (getRenamedModule)
import Elara.SCC (buildSCCs, runFreeVarsQuery, runReachableSubgraphQuery)
import Elara.Settings (CompilerSettings)
import Elara.Shunt (runGetOpInfoQuery, runGetOpTableInQuery, runGetShuntedModuleQuery, runShuntedDeclarationByNameQuery)
import Elara.ToCore (runGetCoreModuleQuery, runGetDataConQuery, runGetTyConQuery)
import Elara.TypeInfer (runGetTypeCheckedModuleQuery, runInferSCCQuery, runKindOfQuery, runTypeOfQuery)
import Print (showPretty)
import Rock qualified
import System.FilePath (takeFileName)
import Prelude hiding (withFile)

rules :: CompilerSettings -> Rock.Rules Query
rules compilerSettings key = do
    case key of
        GetCompilerSettings -> pure compilerSettings
        InputFiles -> inject getInputFiles
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
        ParsedModule mn -> inject $ getParsedModuleQuery mn
        DesugaredModule mn -> inject $ getDesugaredModule mn
        RenamedModule mn -> Local.evalState primitiveRenameState $ inject $ getRenamedModule mn
        ShuntedModule mn -> do
            (mod, warnings) <- runWriter $ inject $ runGetShuntedModuleQuery mn
            traverse_ report warnings
            pure mod
        ShuntedDeclarationByName name -> inject $ runShuntedDeclarationByNameQuery name
        GetOpInfo opName -> inject $ runGetOpInfoQuery opName
        GetOpTableIn mn -> inject $ runGetOpTableInQuery mn
        FreeVarsOf name -> inject $ runFreeVarsQuery name
        ReachableSubgraphOf name -> inject $ runReachableSubgraphQuery name
        GetSCCsOf name -> do
            subgraph <- inject $ runReachableSubgraphQuery name
            pure $ buildSCCs subgraph
        SCCKeyOf _ -> do
            error "SCCKeyOf not implemented"
        TypeCheckedModule mn -> inject $ runGetTypeCheckedModuleQuery mn
        InferSCC sccKey -> inject $ runInferSCCQuery sccKey
        TypeOf key -> inject $ runTypeOfQuery key
        KindOf qtn -> inject $ runKindOfQuery qtn
        GetCoreModule mn -> inject $ runGetCoreModuleQuery mn
        GetTyCon qn -> inject $ runGetTyConQuery qn
        GetDataCon qn -> inject $ runGetDataConQuery qn
        GetOptimisedCoreModule mn -> inject $ runGetOptimisedCoreModuleQuery mn
        GetANFCoreModule mn -> inject $ runGetANFCoreModuleQuery mn
        GetClosureLiftedModule mn -> inject $ runGetClosureLiftedModuleQuery mn
        GetFinalisedCoreModule mn -> inject $ runGetFinalisedCoreModuleQuery mn
