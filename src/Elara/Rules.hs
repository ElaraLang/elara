{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.Rules where

import Data.Generics.Product (HasField' (..))
import Data.Generics.Wrapped (_Unwrapped)
import Data.Text qualified as Text
import Effectful
import Effectful.Exception (throwIO)
import Effectful.State.Static.Local qualified as Local
import Effectful.Writer.Static.Local (runWriter)
import Elara.AST.Generic.Types (ASTLocate, CleanupLocated, RUnlocate (..), Select, declaration'Name, declarationBodyName, declarationName)
import Elara.AST.Module (Module, Module')
import Elara.AST.Name (ModuleName (..), Name, Qualified (..), ToName)
import Elara.AST.Region (Located, unlocated)
import Elara.AST.Select
import Elara.Core.LiftClosures (runGetClosureLiftedModuleQuery)
import Elara.CoreToCore (runGetANFCoreModuleQuery, runGetFinalisedCoreModuleQuery, runGetOptimisedCoreModuleQuery)
import Elara.Data.Pretty
import Elara.Desugar (getDesugaredModule)
import Elara.Desugar.Error
import Elara.Error
import Elara.Error.Internal
import Elara.Lexer.Reader (getLexedFile)
import Elara.Logging (debug)
import Elara.Parse (getParsedFileQuery, getParsedModuleQuery)
import Elara.Prim.Rename (primitiveRenameState)
import Elara.Query
import Elara.Query.Effects (HasMinimumQueryEffects)
import Elara.ReadFile (getInputFiles, runGetFileContentsQuery)
import Elara.Rename (getRenamedModule)
import Elara.SCC (buildSCCs, runFreeVarsQuery, runReachableSubgraphQuery)
import Elara.Settings (CompilerSettings, mainFile)
import Elara.Shunt (runGetOpInfoQuery, runGetOpTableInQuery, runGetShuntedModuleQuery)
import Elara.ToCore (runGetCoreModuleQuery, runGetDataConQuery, runGetTyConQuery)
import Elara.TypeInfer (runGetTypeCheckedModuleQuery, runInferSCCQuery, runKindOfQuery, runTypeCheckedExprQuery, runTypeOfQuery)
import Optics (filtered)
import Print (showPretty)
import Rock qualified
import System.FilePath (takeFileName)

rules :: HasCallStack => CompilerSettings -> Rock.Rules Query
rules compilerSettings key = do
    case key of
        GetCompilerSettings -> pure compilerSettings
        InputFiles -> inject getInputFiles
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
        RenamedModule mn -> Local.evalState primitiveRenameState $ inject $ getRenamedModule mn
        ModuleByName @ast mn -> query @QueryModuleByName @ast mn
        DeclarationByName @ast name -> query @QueryDeclarationByName @ast name
        RequiredDeclarationByName @ast name -> query @QueryRequiredDeclarationByName @ast name
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

instance
    ( SupportsQuery QueryModuleByName ast
    , HasMinimumQueryEffects (QueryEffectsOf QueryDeclarationByName ast)
    , Rock.Rock Query :> QueryEffectsOf QueryDeclarationByName ast
    , Typeable ast
    , Subset
        (QueryEffectsOf QueryModuleByName ast)
        (QueryEffectsOf QueryDeclarationByName ast)
    , QueryReturnTypeOf QueryModuleByName ast ~ Module ast
    , ToName (Select (ASTName ForValueDecl) ast)
    , ToName (Select (ASTName ForType) ast)
    , RUnlocate ast
    , -- TODO these are stupid and we should find a way to remove them
      CleanupLocated (Located (Select (ASTName ForType) ast)) ~ Located (Select (ASTName ForType) ast)
    , CleanupLocated (Located (Select (ASTName ForValueDecl) ast)) ~ Located (Select (ASTName ForValueDecl) ast)
    ) =>
    SupportsQuery QueryDeclarationByName ast
    where
    query (Qualified name modName) = do
        mod :: Module ast <- Rock.fetch $ ModuleByName @ast modName

        let matchingBodies =
                mod
                    ^.. _Unwrapped @(Module ast) @(Module ast) @(ASTLocate ast (Module' ast))
                    % rUnlocated @_ @ast @(Module' ast)
                    % field' @"declarations"
                    % each
                    % filtered (\b -> b ^. (declarationName @ast) % (rUnlocated @_ @ast @Name) == name)

        case matchingBodies of
            [] -> pure Nothing
            [decl] -> pure (Just decl)
            _ -> throwIO $ DuplicateDeclAfterDesugar modName name

instance
    ( SupportsQuery QueryDeclarationByName ast
    , Subset
        (QueryEffectsOf QueryDeclarationByName ast)
        (QueryEffectsOf QueryRequiredDeclarationByName ast)
    , HasMinimumQueryEffects (QueryEffectsOf QueryRequiredDeclarationByName ast)
    , Rock.Rock Query :> QueryEffectsOf QueryRequiredDeclarationByName ast
    , Typeable ast
    ) =>
    SupportsQuery QueryRequiredDeclarationByName ast
    where
    query name = do
        mDecl <- Rock.fetch $ DeclarationByName @ast name
        case mDecl of
            Just decl -> pure decl
            Nothing -> throwIO $ RequiredDeclNotFound name

instance SupportsQuery QueryModuleByName Shunted where
    query mn = do
        (mod, warnings) <- runWriter $ inject $ runGetShuntedModuleQuery mn
        traverse_ report warnings
        pure mod

instance SupportsQuery QueryModuleByName Renamed where
    query mn = Local.evalState primitiveRenameState $ inject $ getRenamedModule mn
