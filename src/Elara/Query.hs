{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Data.Kind (Constraint, Type)
import Data.Kind qualified as Kind
import Effectful
import Effectful.Error.Static (Error)
import Effectful.FileSystem (FileSystem)
import Effectful.Writer.Static.Local
import Elara.AST.Module qualified as NewModule
import Elara.AST.Name (ModuleName, Name, Qualified, TypeName, VarName)
import Elara.AST.Phase (ElaraPhase (..))
import Elara.AST.Phases.Desugared qualified as NewD
import Elara.AST.Phases.Frontend qualified as NewF
import Elara.AST.Phases.Renamed qualified as NewR
import Elara.AST.Phases.Shunted qualified as NewS
import Elara.AST.Phases.Typed (Typed, TypedExpr)
import Elara.AST.Region (SourceRegion)
import Elara.AST.Types qualified as New
import Elara.AST.VarRef (IgnoreLocVarRef)
import Elara.Core (CoreBind, DataCon, TyCon)
import Elara.Core qualified as Core
import Elara.Core.ANF qualified as ANF
import Elara.Core.LiftClosures.Error (ClosureLiftError)
import Elara.Core.Module (CoreModule)
import Elara.Data.Kind (KindVar)
import Elara.Data.Pretty
import Elara.Desugar.Error (DesugarError)
import Elara.Error
import Elara.JVM.Error (JVMLoweringError)
import Elara.JVM.IR qualified as IR
import Elara.Lexer.Token
import Elara.Lexer.Utils (LexerError)
import Elara.ModuleIndex (ModuleIndex)
import Elara.Parse.Error (ElaraParseError, WParseErrorBundle)
import Elara.Parse.Stream (TokenStream)
import Elara.Query.Effects
import Elara.Query.Errors
import Elara.Query.TH
import Elara.ReadFile (FileContents, ModulePathError, ReadFileError)
import Elara.Rename.Error (RenameError)
import Elara.SCC.Type (ReachableSubgraph, SCCKey)
import Elara.Settings (CompilerSettings)
import Elara.Shunt.Error (ShuntError, ShuntWarning)
import Elara.Shunt.Operator (OpInfo, OpTable)
import Elara.TypeInfer.Environment (TypeEnvKey)
import Elara.TypeInfer.Type (Polytype)
import Elara.TypeInfer.Type qualified as Infer
import Elara.TypeInfer.Unique
import JVM.Data.Abstract.ClassFile (ClassFile)
import JVM.Data.Convert.Monad (CodeConverterError)
import Rock (Rock)

type WithRock effects =
    Rock.Rock Elara.Query.Query ': effects

data ASTQueryType = QModule | QDecl | QReqDecl | QCtor | QAnn | QAnnType

class (Typeable ast, ElaraPhase ast) => RunPhase ast where
    type ASTQueryEffects (ast :: Type) (q :: ASTQueryType) :: [Effect]
    type ASTQueryEffects (ast :: Type) (q :: ASTQueryType) = StandardQueryError ast

    getModuleByName ::
        HasCallStack =>
        ModuleName ->
        Eff (WithRock (ConsQueryEffects (ASTQueryEffects ast 'QModule))) (NewModule.Module SourceRegion ast)

    getDeclarationByName ::
        HasCallStack =>
        Qualified Name ->
        Eff (WithRock (ConsQueryEffects (ASTQueryEffects ast 'QDecl))) (Maybe (New.Declaration SourceRegion ast))

    getRequiredDeclarationByName ::
        HasCallStack =>
        Qualified Name ->
        Eff (WithRock (ConsQueryEffects (ASTQueryEffects ast 'QReqDecl))) (New.Declaration SourceRegion ast)

    getConstructorDeclaration ::
        HasCallStack =>
        ConstructorOccurrence ast SourceRegion ->
        Eff (WithRock (ConsQueryEffects (ASTQueryEffects ast 'QCtor))) (New.Declaration SourceRegion ast)

    getDeclarationAnnotations ::
        HasCallStack =>
        Qualified Name ->
        Eff (WithRock (ConsQueryEffects (ASTQueryEffects ast 'QAnn))) [New.Annotation SourceRegion ast]

    getDeclarationAnnotationsOfType ::
        HasCallStack =>
        (Qualified Name, Qualified TypeName) ->
        Eff (WithRock (ConsQueryEffects (ASTQueryEffects ast 'QAnnType))) [New.Annotation SourceRegion ast]

data Query (es :: [Effect]) a where
    -- \* Input Queries

    -- | Query to get the compiler settings
    GetCompilerSettings :: Query (WithRock MinimumQueryEffects) CompilerSettings
    -- | Query to get all the required input files to be passed to the compiler
    InputFiles :: Query (WithRock (ConsMinimumQueryEffects '[FileSystem])) (HashSet FilePath)
    -- | Query to get the module index (bidirectional mapping between file paths and module names)
    ModuleIndex :: Query (WithRock (ConsMinimumQueryEffects '[FileSystem])) ModuleIndex
    -- | Query to get the contents of a specific file
    GetFileContents :: FilePath -> Query (WithRock (ConsMinimumQueryEffects '[FileSystem, Error ReadFileError])) FileContents
    -- | Query to get the file path of a module
    ModulePath :: ModuleName -> Query (WithRock (ConsQueryEffects '[Error ModulePathError])) FilePath
    -- \* Lexing and Parsing Queries

    -- | Query to get the lexed tokens of a specific file
    LexedFile :: FilePath -> Query (WithRock (ConsQueryEffects '[Error LexerError])) [Lexeme]
    -- | Query to get the parsed module from a file's contents and lexed tokens
    ParsedFile :: FilePath -> Query (WithRock (ConsQueryEffects '[Error (WParseErrorBundle TokenStream ElaraParseError)])) (NewModule.Module SourceRegion NewF.Frontend)
    -- | Query to get a parsed module by module name
    ParsedModule ::
        ModuleName ->
        Query
            (WithRock (ConsQueryEffects '[Error (WParseErrorBundle TokenStream ElaraParseError)]))
            (NewModule.Module SourceRegion NewF.Frontend)
    -- \* Desugaring and Renaming Queries
    DesugaredModule ::
        ModuleName ->
        Query (WithRock (ConsQueryEffects '[Error DesugarError])) (NewModule.Module SourceRegion NewD.Desugared)
    -- \* Phase-Polymorphic AST Queries
    ModuleByName ::
        (RunPhase ast, Typeable ast) =>
        ModuleName ->
        Query (WithRock (ConsQueryEffects (ASTQueryEffects ast 'QModule))) (NewModule.Module SourceRegion ast)
    DeclarationByName ::
        (RunPhase ast, Typeable ast) =>
        Qualified Name ->
        Query (WithRock (ConsQueryEffects (ASTQueryEffects ast 'QDecl))) (Maybe (New.Declaration SourceRegion ast))
    RequiredDeclarationByName ::
        (RunPhase ast, Typeable ast) =>
        Qualified Name ->
        Query (WithRock (ConsQueryEffects (ASTQueryEffects ast 'QReqDecl))) (New.Declaration SourceRegion ast)
    ConstructorDeclaration ::
        (RunPhase ast, Typeable ast, Show (ConstructorOccurrence ast SourceRegion), Ord (ConstructorOccurrence ast SourceRegion), Hashable (ConstructorOccurrence ast SourceRegion)) =>
        ConstructorOccurrence ast SourceRegion ->
        Query (WithRock (ConsQueryEffects (ASTQueryEffects ast 'QCtor))) (New.Declaration SourceRegion ast)
    DeclarationAnnotations ::
        (RunPhase ast, Typeable ast) =>
        Qualified Name ->
        Query (WithRock (ConsQueryEffects (ASTQueryEffects ast 'QAnn))) [New.Annotation SourceRegion ast]
    DeclarationAnnotationsOfType ::
        (RunPhase ast, Typeable ast) =>
        (Qualified Name, Qualified TypeName) ->
        Query (WithRock (ConsQueryEffects (ASTQueryEffects ast 'QAnnType))) [New.Annotation SourceRegion ast]
    --    \* Shunting Queries
    GetOpInfo :: IgnoreLocVarRef Name -> Query (WithRock (ConsQueryEffects '[Writer (Set ShuntWarning), Error ShuntError])) (Maybe OpInfo)
    GetOpTableIn :: ModuleName -> Query (WithRock (ConsQueryEffects '[])) OpTable
    -- \* Pre-Inference Queries
    -- These are related to preparing SCCs etc for type inference
    FreeVarsOf :: Qualified VarName -> Query (WithRock (ConsQueryEffects '[])) (HashSet (Qualified VarName))
    ReachableSubgraphOf :: Qualified VarName -> Query (WithRock (ConsQueryEffects '[])) ReachableSubgraph
    GetSCCsOf :: Qualified VarName -> Query (WithRock (ConsQueryEffects '[])) [SCC (Qualified VarName)]
    SCCKeyOf :: Qualified VarName -> Query (WithRock (ConsQueryEffects '[])) SCCKey
    -- \* Type and Kind Inference Queries
    TypeCheckedExpr :: Qualified VarName -> Query (WithRock (ConsQueryEffects '[])) TypedExpr
    TypeOf :: loc ~ SourceRegion => TypeEnvKey loc -> Query (WithRock (ConsQueryEffects '[])) (Infer.Type loc)
    InferSCC :: SCCKey -> Query (WithRock (ConsQueryEffects '[])) (Map (Qualified VarName) (Polytype SourceRegion))
    KindOf :: Qualified TypeName -> Query (WithRock (ConsQueryEffects '[])) (Maybe KindVar)
    -- | Get Information about a type alias
    GetTypeAlias ::
        -- | The name of the type alias
        Qualified TypeName ->
        -- | The type alias's type variables and body, if it exists
        Query (WithRock (ConsQueryEffects '[])) (Maybe ([UniqueTyVar], Infer.Type SourceRegion))
    GetCoreModule :: ModuleName -> Query (WithRock (ConsQueryEffects '[])) (CoreModule CoreBind)
    GetTyCon :: Qualified Text -> Query (WithRock (ConsQueryEffects '[])) (Maybe TyCon)
    GetDataCon :: Qualified TypeName -> Query (WithRock (ConsQueryEffects '[])) (Maybe DataCon)
    TypeCheckedDeclaration :: Qualified Name -> Query (WithRock (ConsQueryEffects '[])) (New.Declaration SourceRegion Typed)
    -- \* Core To Core
    GetOptimisedCoreModule :: ModuleName -> Query (WithRock (ConsQueryEffects '[])) (CoreModule CoreBind)
    GetANFCoreModule :: ModuleName -> Query (WithRock (ConsQueryEffects '[])) (CoreModule (ANF.TopLevelBind Core.Var))
    GetClosureLiftedModule :: ModuleName -> Query (WithRock (ConsQueryEffects '[Error ClosureLiftError])) (CoreModule (ANF.TopLevelBind Core.Var))
    GetFinalisedCoreModule :: ModuleName -> Query (WithRock (ConsQueryEffects '[])) (CoreModule CoreBind)
    -- \* JVM Backend Queries

    GetJVMIRModule :: ModuleName -> Query (WithRock (ConsQueryEffects '[Error JVMLoweringError])) IR.Module
    GetJVMClassFiles :: ModuleName -> Query (WithRock (ConsQueryEffects '[Error JVMLoweringError])) [ClassFile]
    GetJVMClassBytes :: ModuleName -> Query (WithRock (ConsQueryEffects '[Error JVMLoweringError, Error CodeConverterError])) [(FilePath, LByteString)]

instance GEq (Query es) => Eq (Query es a) where
    x == y = case geq x y of
        Just Refl -> True
        Nothing -> False

$(makeTag ''Query)
$(deriveSameCtor ''Query)

deriving instance Show (Query es a)

instance GCompare (Query es) => GEq (Query es) where
    geq x y = case gcompare x y of
        GEQ -> Just Refl
        _ -> Nothing

instance GCompare (Query es) where
    gcompare a b =
        case compare (tagQuery a) (tagQuery b) of
            LT -> GLT
            GT -> GGT
            EQ -> sameCtor a b

$(deriveHashableInstance ''Query)
