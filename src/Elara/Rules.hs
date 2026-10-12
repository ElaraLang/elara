{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.Rules where

import System.FilePath (takeDirectory, (</>))

import Data.Text qualified as Text

import Elara.AST.Module (Module (..), Module' (..))
import Elara.AST.Name (ModuleName (..))
import Elara.AST.Region (unlocated)
import Elara.Core.LiftClosures (runGetClosureLiftedModuleQuery)
import Elara.CoreToCore (runGetANFCoreModuleQuery, runGetFinalisedCoreModuleQuery, runGetOptimisedCoreModuleQuery)
import Elara.Desugar (getDesugaredModule)
import Elara.Error (throwElaraError)
import Elara.Lexer.Reader (getLexedFile)
import Elara.ModuleIndex (ModuleEntry (..), buildModuleIndex)
import Elara.Parse (getParsedFileQuery, getParsedModuleQuery)
import Elara.Query
import Elara.ReadFile (ModulePathError (..), getInputFiles, runGetFileContentsQuery)
import Elara.SCC (buildSCCs, runFreeVarsQuery, runReachableSubgraphQuery)
import Elara.Settings (CompilerSettings (..), mainFile)
import Elara.Shunt (runGetOpInfoQuery, runGetOpTableInQuery)
import Elara.ToCore (runGetCoreModuleQuery, runGetDataConQuery, runGetTyConQuery)
import Elara.TypeInfer (runGetTypeAliasQuery, runInferSCCQuery, runKindOfQuery, runTypeCheckedDeclarationQuery, runTypeCheckedExprQuery, runTypeOfQuery)

import Elara.JVM.Query qualified
import Elara.ModuleIndex qualified as ModuleIndex
import Rock qualified

rules :: HasCallStack => CompilerSettings -> Rock.Rules Query
rules compilerSettings key = case key of
    GetCompilerSettings -> pure compilerSettings
    InputFiles -> getInputFiles compilerSettings
    ModuleIndex -> buildModuleIndex compilerSettings
    GetFileContents fp -> runGetFileContentsQuery fp
    LexedFile fp -> getLexedFile fp
    ParsedFile fp -> getParsedFileQuery fp
    ModulePath mn -> do
        -- Check main file first (parses main file to check declared name)
        let checkMain = case compilerSettings.mainFile of
                Nothing -> pure Nothing
                Just mainPath -> do
                    Module _ m <- Rock.fetch (Elara.Query.ParsedFile mainPath)
                    pure (if moduleName m ^. unlocated == mn then Just mainPath else Nothing)

        maybeMain <- checkMain
        case maybeMain of
            Just p -> pure p
            Nothing -> do
                -- Use the module index for lookup
                idx <- Rock.fetch Elara.Query.ModuleIndex
                case ModuleIndex.lookupModule mn idx of
                    Nothing -> throwElaraError $ ModuleNotFound mn (generateSearchPaths mn)
                    Just [] -> throwElaraError $ ModuleNotFound mn []
                    Just [single] -> pure single.entryPath
                    Just multiple -> throwElaraError $ MultipleModulePaths mn (map (.entryPath) multiple)
      where
        -- Generate search paths for error messages
        generateSearchPaths :: ModuleName -> [FilePath]
        generateSearchPaths (ModuleName parts) =
            let sourceDirs = compilerSettings.sourceDirs
                mainFileDir = case compilerSettings.mainFile of
                    Just fp -> [takeDirectory fp]
                    Nothing -> []
                roots = ordNub $ ["stdlib"] <> sourceDirs <> mainFileDir
                hierarchy = Text.intercalate "/" (toList parts)
                flat = Text.intercalate "." (toList parts)
                paths =
                    [ toString hierarchy <> ".elr"
                    , toString hierarchy </> "mod.elr"
                    , toString flat <> ".elr"
                    ]
             in ordNub [root </> path | root <- roots, path <- paths]
    ParsedModule mn -> getParsedModuleQuery mn
    DesugaredModule mn -> getDesugaredModule mn
    ModuleByName @ast mn -> getModuleByName @ast mn
    DeclarationByName @ast name -> getDeclarationByName @ast name
    RequiredDeclarationByName @ast name -> getRequiredDeclarationByName @ast name
    ConstructorDeclaration @ast conRef -> getConstructorDeclaration @ast conRef
    DeclarationAnnotations @ast name -> getDeclarationAnnotations @ast name
    DeclarationAnnotationsOfType @ast (name, annotationName) -> getDeclarationAnnotationsOfType @ast (name, annotationName)
    GetOpInfo opName -> runGetOpInfoQuery opName
    GetOpTableIn mn -> runGetOpTableInQuery mn
    FreeVarsOf name -> runFreeVarsQuery name
    ReachableSubgraphOf name -> runReachableSubgraphQuery name
    GetSCCsOf name -> do
        subgraph <- runReachableSubgraphQuery name
        pure $ buildSCCs subgraph
    SCCKeyOf _ -> error "SCCKeyOf not implemented"
    TypeCheckedExpr exprId -> runTypeCheckedExprQuery exprId
    TypeCheckedDeclaration name -> runTypeCheckedDeclarationQuery name
    InferSCC sccKey -> runInferSCCQuery sccKey
    TypeOf key -> runTypeOfQuery key
    KindOf qtn -> runKindOfQuery qtn
    GetTypeAlias qtn -> runGetTypeAliasQuery qtn
    GetCoreModule mn -> runGetCoreModuleQuery mn
    GetTyCon qn -> runGetTyConQuery qn
    GetDataCon qn -> runGetDataConQuery qn
    GetOptimisedCoreModule mn -> runGetOptimisedCoreModuleQuery mn
    GetANFCoreModule mn -> runGetANFCoreModuleQuery mn
    GetClosureLiftedModule mn -> runGetClosureLiftedModuleQuery mn
    GetFinalisedCoreModule mn -> runGetFinalisedCoreModuleQuery mn
    -- JVM Backend Queries
    GetJVMIRModule mn -> Elara.JVM.Query.runGetJVMIRModuleQuery mn
    GetJVMClassFiles mn -> Elara.JVM.Query.runGetJVMClassFilesQuery mn
    GetJVMClassBytes mn -> Elara.JVM.Query.runGetJVMClassBytesQuery mn
