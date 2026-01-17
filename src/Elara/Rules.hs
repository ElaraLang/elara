{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.Rules where

import Data.Text qualified as Text
import Effectful
import Effectful.State.Static.Local qualified as Local
import Elara.AST.Name (ModuleName (..), Qualified (..))
import Elara.AST.Select
import Elara.Core.LiftClosures (runGetClosureLiftedModuleQuery)
import Elara.CoreToCore (runGetANFCoreModuleQuery, runGetFinalisedCoreModuleQuery, runGetOptimisedCoreModuleQuery)
import Elara.Data.Pretty
import Elara.Desugar (getDesugaredModule)
import Elara.Lexer.Reader (getLexedFile)
import Elara.Logging (debug, logDebug, withNamespace, withOverriddenNamespace)
import Elara.Parse (getParsedFileQuery, getParsedModuleQuery)
import Elara.Prim.Rename (primitiveRenameState)
import Elara.Query
import Elara.ReadFile (getInputFiles, runGetFileContentsQuery)
import Elara.Rename (getRenamedModule)
import Elara.SCC (buildSCCs, runFreeVarsQuery, runReachableSubgraphQuery)
import Elara.Settings (CompilerSettings, mainFile)
import Elara.Shunt (runGetOpInfoQuery, runGetOpTableInQuery)
import Elara.ToCore (runGetCoreModuleQuery, runGetDataConQuery, runGetTyConQuery)
import Elara.TypeInfer (runGetTypeAliasQuery, runGetTypeCheckedModuleQuery, runInferSCCQuery, runKindOfQuery, runTypeCheckedDeclarationQuery, runTypeCheckedExprQuery, runTypeOfQuery)
import Print (showPretty)
import Rock qualified
import System.FilePath (takeFileName)

rules :: HasCallStack => CompilerSettings -> Rock.Rules Query
rules compilerSettings key = do
    case key of
        GetCompilerSettings -> pure compilerSettings
        InputFiles -> inject (getInputFiles compilerSettings)
        GetFileContents fp -> runGetFileContentsQuery fp
        LexedFile fp -> inject $ getLexedFile fp
        ParsedFile fp -> inject $ getParsedFileQuery fp
        ModulePath (ModuleName ("Main" :| [])) -> do
            pure (compilerSettings.mainFile ?: "source.elr") -- THIS IS BAD
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
        RenamedModule mn ->
            Local.evalState primitiveRenameState $
                inject $
                    withOverriddenNamespace ["Rename"] $
                        getRenamedModule mn
        ModuleByName @ast mn -> query @QueryModuleByName @ast mn
        DeclarationByName @ast name -> query @QueryDeclarationByName @ast name
        RequiredDeclarationByName @ast name -> query @QueryRequiredDeclarationByName @ast name
        ConstructorDeclaration @ast conRef -> query @QueryConstructorDeclaration @ast conRef
        DeclarationAnnotations @ast name -> query @DeclarationAnnotations @ast name
        DeclarationAnnotationsOfType @ast (name, annotationName) -> query @DeclarationAnnotationsOfType @ast (name, annotationName)
        GetOpInfo opName -> inject $ runGetOpInfoQuery opName
        GetOpTableIn mn -> inject $ runGetOpTableInQuery mn
        FreeVarsOf name -> inject $ runFreeVarsQuery name
        ReachableSubgraphOf name -> inject $ runReachableSubgraphQuery name
        GetSCCsOf name -> do
            subgraph <- inject $ runReachableSubgraphQuery name
            debug $ "Subgraph for " <> pretty name <> ": " <> pretty subgraph
            pure $ buildSCCs subgraph
        SCCKeyOf _ -> do
            error "SCCKeyOf not implemented"
        TypeCheckedModule mn -> inject $ runGetTypeCheckedModuleQuery mn
        TypeCheckedExpr exprId -> inject $ runTypeCheckedExprQuery exprId
        TypeCheckedDeclaration name -> inject $ runTypeCheckedDeclarationQuery name
        InferSCC sccKey -> inject $ runInferSCCQuery sccKey
        TypeOf key -> inject $ withOverriddenNamespace ["TypeInfer"] $ runTypeOfQuery key
        KindOf qtn -> inject $ withOverriddenNamespace ["TypeInfer", "Kind"] $ runKindOfQuery qtn
        GetTypeAlias qtn -> inject $ withOverriddenNamespace ["TypeInfer"] $ runGetTypeAliasQuery qtn
        GetCoreModule mn -> inject $ withOverriddenNamespace ["ToCore"] $ runGetCoreModuleQuery mn
        GetTyCon qn -> inject $ withOverriddenNamespace ["ToCore"] $ runGetTyConQuery qn
        GetDataCon qn -> inject $ withOverriddenNamespace ["ToCore"] $ runGetDataConQuery qn
        GetOptimisedCoreModule mn -> inject $ withOverriddenNamespace ["ToCore", "Optimised"] $ runGetOptimisedCoreModuleQuery mn
        GetANFCoreModule mn -> inject $ withOverriddenNamespace ["ToCore", "ANF"] $ runGetANFCoreModuleQuery mn
        GetClosureLiftedModule mn -> inject $ withOverriddenNamespace ["ToCore", "ClosureLifted"] $ runGetClosureLiftedModuleQuery mn
        GetFinalisedCoreModule mn -> inject $ withOverriddenNamespace ["ToCore", "TypeCheck"] $ runGetFinalisedCoreModuleQuery mn
