{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Elara.Query
Description: This module defines the queries used in the Elara compiler.

    Queries are the way the compiler requests information between stages.
    Queries are lazy and memoised.

    This module defines the queries, and the main implementation is in 'Elara.Rules'
-}
module Elara.Query where

import Data.Data (type (:~:) (Refl))
import Data.GADT.Compare
import Data.Graph (SCC)
import Data.Kind (Type)
import Effectful
import H2JVM

import Elara.AST.Name (ModuleName, Name, Qualified, TypeName, VarName)
import Elara.AST.Phase (ElaraPhase (..))
import Elara.AST.Phases.Typed (Typed, TypedExpr)
import Elara.AST.Region (SourceRegion)
import Elara.Core (CoreBind, DataCon, TyCon)
import Elara.Core.Module (CoreModule)
import Elara.Data.Kind (KindVar)
import Elara.Lexer.Token
import Elara.ModuleIndex (ModuleIndex)
import Elara.Query.Effects
import Elara.Query.TH
import Elara.ReadFile (FileContents)
import Elara.SCC.Type (ReachableSubgraph, SCCKey)
import Elara.Settings (CompilerSettings)
import Elara.Shunt.Operator (DeclaredFixity, OpTable)
import Elara.TypeInfer.Environment (TypeEnvKey)
import Elara.TypeInfer.Type (Polytype)
import Elara.TypeInfer.Unique
import Rock (Rock)

import Elara.AST.Module qualified as NewModule
import Elara.AST.Phases.Desugared qualified as NewD
import Elara.AST.Phases.Frontend qualified as NewF
import Elara.AST.Types qualified as New
import Elara.Core qualified as Core
import Elara.Core.ANF qualified as ANF
import Elara.JVM.IR qualified as IR
import Elara.TypeInfer.Type qualified as Infer

class (Typeable ast, ElaraPhase ast) => RunPhase (ast :: Type) where
    getModuleByName ::
        (HasCallStack, QueryEffects es, Rock Query :> es) =>
        ModuleName ->
        Eff es (NewModule.Module SourceRegion ast)

    getDeclarationByName ::
        (HasCallStack, QueryEffects es, Rock Query :> es) =>
        Qualified Name ->
        Eff es (Maybe (New.Declaration SourceRegion ast))

    getRequiredDeclarationByName ::
        (HasCallStack, QueryEffects es, Rock Query :> es) =>
        Qualified Name ->
        Eff es (New.Declaration SourceRegion ast)

    getConstructorDeclaration ::
        (HasCallStack, QueryEffects es, Rock Query :> es) =>
        Qualified TypeName ->
        Eff es (Maybe (New.Declaration SourceRegion ast))

    getDeclarationAnnotations ::
        (HasCallStack, QueryEffects es, Rock Query :> es) =>
        Qualified Name ->
        Eff es [New.Annotation SourceRegion ast]

    getDeclarationAnnotationsOfType ::
        (HasCallStack, QueryEffects es, Rock Query :> es) =>
        (Qualified Name, Qualified TypeName) ->
        Eff es [New.Annotation SourceRegion ast]

data Query a where
    -- \* Input Queries

    -- | Query to get the compiler settings
    GetCompilerSettings :: Query CompilerSettings
    -- | Query to get all the required input files to be passed to the compiler
    InputFiles :: Query (HashSet FilePath)
    -- | Query to get the module index (bidirectional mapping between file paths and module names)
    ModuleIndex :: Query ModuleIndex
    -- | Query to get the contents of a specific file
    GetFileContents :: FilePath -> Query FileContents
    -- | Query to get the file path of a module
    ModulePath :: ModuleName -> Query FilePath
    -- \* Lexing and Parsing Queries

    -- | Query to get the lexed tokens of a specific file
    LexedFile :: FilePath -> Query [Lexeme]
    -- | Query to get the parsed module from a file's contents and lexed tokens
    ParsedFile :: FilePath -> Query (NewModule.Module SourceRegion NewF.Frontend)
    -- | Query to get a parsed module by module name
    ParsedModule ::
        ModuleName ->
        Query (NewModule.Module SourceRegion NewF.Frontend)
    -- \* Desugaring and Renaming Queries
    DesugaredModule ::
        ModuleName ->
        Query (NewModule.Module SourceRegion NewD.Desugared)
    -- \* Phase-Polymorphic AST Queries
    ModuleByName ::
        (RunPhase ast, Typeable ast) =>
        ModuleName ->
        Query (NewModule.Module SourceRegion ast)
    DeclarationByName ::
        (RunPhase ast, Typeable ast) =>
        Qualified Name ->
        Query (Maybe (New.Declaration SourceRegion ast))
    RequiredDeclarationByName ::
        (RunPhase ast, Typeable ast) =>
        Qualified Name ->
        Query (New.Declaration SourceRegion ast)
    ConstructorDeclaration ::
        (RunPhase ast, Typeable ast) =>
        Qualified TypeName ->
        Query (Maybe (New.Declaration SourceRegion ast))
    DeclarationAnnotations ::
        (RunPhase ast, Typeable ast) =>
        Qualified Name ->
        Query [New.Annotation SourceRegion ast]
    DeclarationAnnotationsOfType ::
        (RunPhase ast, Typeable ast) =>
        (Qualified Name, Qualified TypeName) ->
        Query [New.Annotation SourceRegion ast]
    --    \* Shunting Queries
    GetOpInfo :: Qualified Name -> Query DeclaredFixity
    GetOpTableIn :: ModuleName -> Query OpTable
    -- \* Pre-Inference Queries
    -- These are related to preparing SCCs etc for type inference
    FreeVarsOf :: Qualified VarName -> Query (HashSet (Qualified VarName))
    ReachableSubgraphOf :: Qualified VarName -> Query ReachableSubgraph
    GetSCCsOf :: Qualified VarName -> Query [SCC (Qualified VarName)]
    SCCKeyOf :: Qualified VarName -> Query SCCKey
    -- \* Type and Kind Inference Queries
    TypeCheckedExpr :: Qualified VarName -> Query TypedExpr
    TypeOf :: TypeEnvKey -> Query (Infer.Type SourceRegion)
    InferSCC :: SCCKey -> Query (Map (Qualified VarName) (Polytype SourceRegion))
    KindOf :: Qualified TypeName -> Query (Maybe KindVar)
    -- | Get Information about a type alias
    GetTypeAlias ::
        -- | The name of the type alias
        Qualified TypeName ->
        -- | The type alias's type variables and body, if it exists
        Query (Maybe ([UniqueTyVar], Infer.Type SourceRegion))
    GetCoreModule :: ModuleName -> Query (CoreModule CoreBind)
    GetTyCon :: Qualified Text -> Query (Maybe TyCon)
    GetDataCon :: Qualified TypeName -> Query (Maybe DataCon)
    TypeCheckedDeclaration :: Qualified Name -> Query (New.Declaration SourceRegion Typed)
    -- \* Core To Core
    GetOptimisedCoreModule :: ModuleName -> Query (CoreModule CoreBind)
    GetANFCoreModule :: ModuleName -> Query (CoreModule (ANF.TopLevelBind Core.Var))
    GetClosureLiftedModule :: ModuleName -> Query (CoreModule (ANF.TopLevelBind Core.Var))
    GetFinalisedCoreModule :: ModuleName -> Query (CoreModule CoreBind)
    -- \* JVM Backend Queries

    GetJVMIRModule :: ModuleName -> Query IR.Module
    GetJVMClassFiles :: ModuleName -> Query [ClassFile]
    GetJVMClassBytes :: ModuleName -> Query [(FilePath, LByteString)]

instance GEq Query => Eq (Query a) where
    x == y = case geq x y of
        Just Refl -> True
        Nothing -> False

$(makeTag ''Query)
$(deriveSameCtor ''Query)

deriving instance Show (Query a)

instance GCompare Query => GEq Query where
    geq x y = case gcompare x y of
        GEQ -> Just Refl
        _ -> Nothing

instance GCompare Query where
    gcompare a b =
        case compare (tagQuery a) (tagQuery b) of
            LT -> GLT
            GT -> GGT
            EQ -> sameCtor a b

$(deriveHashableInstance ''Query)
$(deriveKeyChecks ''Query)
