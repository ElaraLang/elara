{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.Rules where

import Data.Generics.Product (field')
import Data.Text qualified as Text
import Effectful
import Effectful.Error.Static (runError, throwError)
import Effectful.State.Static.Local qualified as Local
import Elara.AST.Module (Module (..))
import Elara.AST.Name (ModuleName (..))
import Elara.AST.Region (Located (Located), unlocated)
import Elara.Core.LiftClosures (runGetClosureLiftedModuleQuery)
import Elara.CoreToCore (runGetANFCoreModuleQuery, runGetFinalisedCoreModuleQuery, runGetOptimisedCoreModuleQuery)
import Elara.Desugar (getDesugaredModule)
import Elara.Lexer.Reader (getLexedFile)
import Elara.ModuleIndex (ModuleEntry (..), buildModuleIndex)
import Elara.ModuleIndex qualified as ModuleIndex
import Elara.Parse (getParsedFileQuery, getParsedModuleQuery)
import Elara.Parse.Error (ElaraParseError, WParseErrorBundle)
import Elara.Parse.Stream (TokenStream)
import Elara.Prim.Rename (primitiveRenameState)
import Elara.Query
import Elara.ReadFile (ModulePathError (..), getInputFiles, runGetFileContentsQuery)
import Elara.Rename (getRenamedModule)
import Elara.SCC (buildSCCs, runFreeVarsQuery, runReachableSubgraphQuery)
import Elara.Settings (CompilerSettings (..), mainFile)
import Elara.Shunt (runGetOpInfoQuery, runGetOpTableInQuery)
import Elara.ToCore (runGetCoreModuleQuery, runGetDataConQuery, runGetTyConQuery)
import Elara.TypeInfer (runGetTypeAliasQuery, runGetTypeCheckedModuleQuery, runInferSCCQuery, runKindOfQuery, runTypeCheckedDeclarationQuery, runTypeCheckedExprQuery, runTypeOfQuery)
import Rock qualified
import System.FilePath (takeDirectory, (</>))

rules :: HasCallStack => CompilerSettings -> Rock.Rules Query
rules compilerSettings key = case key of
    GetCompilerSettings -> pure compilerSettings
    InputFiles -> inject (getInputFiles compilerSettings)
    ModuleIndex -> inject (buildModuleIndex compilerSettings)
    GetFileContents fp -> runGetFileContentsQuery fp
    LexedFile fp -> inject $ getLexedFile fp
    ParsedFile fp -> inject $ getParsedFileQuery fp
    ModulePath mn -> do
        -- Check main file first (parses main file to check declared name)
        let checkMain = case compilerSettings.mainFile of
                Nothing -> pure Nothing
                Just mainPath -> do
                    parsedOrError <-
                        runError @(WParseErrorBundle TokenStream ElaraParseError) $
                            Rock.fetch (Elara.Query.ParsedFile mainPath)
                    case parsedOrError of
                        Left _ -> pure Nothing
                        Right (Module (Located _ m)) ->
                            if m ^. field' @"name" % unlocated == mn
                                then pure (Just mainPath)
                                else pure Nothing

        maybeMain <- checkMain
        case maybeMain of
            Just p -> pure p
            Nothing -> do
                -- Use the module index for lookup
                idx <- Rock.fetch Elara.Query.ModuleIndex
                case ModuleIndex.lookupModule mn idx of
                    Nothing -> throwError $ ModuleNotFound mn (generateSearchPaths mn)
                    Just [] -> throwError $ ModuleNotFound mn []
                    Just [single] -> pure single.entryPath
                    Just multiple -> throwError $ MultipleModulePaths mn (map (.entryPath) multiple)
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
    ParsedModule mn -> inject $ getParsedModuleQuery mn
    DesugaredModule mn -> inject $ getDesugaredModule mn
    RenamedModule mn ->
        Local.evalState primitiveRenameState $
            inject $
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
        pure $ buildSCCs subgraph
    SCCKeyOf _ -> error "SCCKeyOf not implemented"
    TypeCheckedModule mn -> inject $ runGetTypeCheckedModuleQuery mn
    TypeCheckedExpr exprId -> inject $ runTypeCheckedExprQuery exprId
    TypeCheckedDeclaration name -> inject $ runTypeCheckedDeclarationQuery name
    InferSCC sccKey -> inject $ runInferSCCQuery sccKey
    TypeOf key -> inject $ runTypeOfQuery key
    KindOf qtn -> inject $ runKindOfQuery qtn
    GetTypeAlias qtn -> inject $ runGetTypeAliasQuery qtn
    GetCoreModule mn -> inject $ runGetCoreModuleQuery mn
    GetTyCon qn -> inject $ runGetTyConQuery qn
    GetDataCon qn -> inject $ runGetDataConQuery qn
    GetOptimisedCoreModule mn -> inject $ runGetOptimisedCoreModuleQuery mn
    GetANFCoreModule mn -> inject $ runGetANFCoreModuleQuery mn
    GetClosureLiftedModule mn -> inject $ runGetClosureLiftedModuleQuery mn
    GetFinalisedCoreModule mn -> inject $ runGetFinalisedCoreModuleQuery mn
