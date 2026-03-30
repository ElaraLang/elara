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

import Effectful
import Effectful.FileSystem (FileSystem)
import Elara.Query.TH

import Data.Data (type (:~:) (Refl))
import Data.GADT.Compare
import Data.Graph (SCC)
import Data.Kind (Constraint)
import Data.Kind qualified as Kind
import Effectful.Error.Static (Error)
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
import Elara.Error (DiagnosticWriter)
import Elara.JVM.Error (JVMLoweringError)
import Elara.JVM.IR qualified as IR
import Elara.Lexer.Token
import Elara.Lexer.Utils (LexerError)
import Elara.ModuleIndex (ModuleIndex)
import Elara.Parse.Error (ElaraParseError, WParseErrorBundle)
import Elara.Parse.Stream (TokenStream)
import Elara.Query.Effects
import Elara.Query.Errors
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

{- | The effects required to run a specific query
| This should probably be moved to the 'SupportsQuery' class as an associated type
-}
type family QueryEffectsOf (q :: QueryType) ast = es where
    QueryEffectsOf q ast = WithRock (ConsQueryEffects (QuerySpecificEffectsOf q ast))

data Query (es :: [Effect]) a where
    -- \* Input Queries

    -- | Query to get the compiler settings
    GetCompilerSettings :: Query (WithRock MinimumQueryEffects) CompilerSettings
    -- | Query to get all the required input files to be passed to the compiler
    InputFiles :: Query (WithRock (ConsMinimumQueryEffects '[FileSystem])) (HashSet FilePath)
    -- | Query to get the module index (bidirectional mapping between file paths and module names)
    ModuleIndex :: Query (WithRock (ConsMinimumQueryEffects '[FileSystem])) ModuleIndex
    -- | Query to get the contents of a specific file
    GetFileContents :: FilePath -> Query (WithRock (ConsMinimumQueryEffects '[FileSystem, Error ReadFileError, DiagnosticWriter (Doc AnsiStyle)])) FileContents
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
    RenamedModule :: ModuleName -> Query (WithRock (ConsQueryEffects '[Error RenameError])) (NewModule.Module SourceRegion NewR.Renamed)
    ShuntedModule ::
        ModuleName ->
        Query (WithRock (ConsQueryEffects '[Writer (Set ShuntWarning), Error ShuntError])) (NewModule.Module SourceRegion NewS.Shunted)
    -- | Lookup a module by name
    ModuleByName ::
        forall ast.
        ( Typeable ast
        , SupportsQuery QueryModuleByName ast
        , HasMinimumQueryEffects (QueryEffectsOf QueryModuleByName ast)
        ) =>
        ModuleName -> Query (QueryEffectsOf QueryModuleByName ast) (QueryReturnTypeOf QueryModuleByName ast)
    --  | Lookup a declaration by name, which may not exist
    DeclarationByName ::
        forall ast.
        ( Typeable ast
        , SupportsQuery QueryDeclarationByName ast
        ) =>
        QueryArgsOf QueryDeclarationByName ast ->
        Query
            (QueryEffectsOf QueryDeclarationByName ast)
            (QueryReturnTypeOf QueryDeclarationByName ast)
    --  | Lookup a declaration by name, which must exist, throwing an 'InternalError' if not found
    RequiredDeclarationByName ::
        forall ast.
        ( Typeable ast
        , SupportsQuery QueryRequiredDeclarationByName ast
        ) =>
        QueryArgsOf QueryRequiredDeclarationByName ast ->
        Query
            (QueryEffectsOf QueryRequiredDeclarationByName ast)
            (QueryReturnTypeOf QueryRequiredDeclarationByName ast)
    -- | Looks up the declaration for a data constructor
    ConstructorDeclaration ::
        forall ast.
        (Typeable ast, Show (ConstructorOccurrence ast SourceRegion), Ord (ConstructorOccurrence ast SourceRegion), Hashable (ConstructorOccurrence ast SourceRegion), ElaraPhase ast, SupportsQuery QueryConstructorDeclaration ast) =>
        QueryArgsOf QueryConstructorDeclaration ast ->
        Query (QueryEffectsOf QueryConstructorDeclaration ast) (QueryReturnTypeOf QueryConstructorDeclaration ast)
    DeclarationAnnotations ::
        forall ast.
        (Typeable ast, SupportsQuery DeclarationAnnotations ast) =>
        QueryArgsOf QueryDeclarationByName ast ->
        Query (QueryEffectsOf DeclarationAnnotations ast) (QueryReturnTypeOf DeclarationAnnotations ast)
    DeclarationAnnotationsOfType ::
        forall ast.
        ( Typeable ast
        , SupportsQuery DeclarationAnnotationsOfType ast
        ) =>
        QueryArgsOf DeclarationAnnotationsOfType ast ->
        Query
            (QueryEffectsOf DeclarationAnnotationsOfType ast)
            (QueryReturnTypeOf DeclarationAnnotationsOfType ast)
    -- \* Shunting Queries
    GetOpInfo :: IgnoreLocVarRef Name -> Query (WithRock (ConsQueryEffects '[Writer (Set ShuntWarning), Error ShuntError])) (Maybe OpInfo)
    GetOpTableIn :: ModuleName -> Query (WithRock (ConsQueryEffects '[])) OpTable
    -- \* Pre-Inference Queries
    -- These are related to preparing SCCs etc for type inference
    FreeVarsOf :: Qualified VarName -> Query (WithRock (ConsQueryEffects '[])) (HashSet (Qualified VarName))
    ReachableSubgraphOf :: Qualified VarName -> Query (WithRock (ConsQueryEffects '[])) ReachableSubgraph
    GetSCCsOf :: Qualified VarName -> Query (WithRock (ConsQueryEffects '[])) [SCC (Qualified VarName)]
    SCCKeyOf :: Qualified VarName -> Query (WithRock (ConsQueryEffects '[])) SCCKey
    -- \* Type and Kind Inference Queries
    TypeCheckedModule :: ModuleName -> Query (WithRock (ConsQueryEffects '[])) (NewModule.Module SourceRegion Typed)
    TypeCheckedExpr :: Qualified VarName -> Query (WithRock (ConsQueryEffects '[])) TypedExpr
    TypeOf :: loc ~ SourceRegion => TypeEnvKey loc -> Query (WithRock (ConsQueryEffects '[])) (Infer.Type loc)
    InferSCC :: SCCKey -> Query (WithRock (ConsQueryEffects '[])) (Map (Qualified VarName) (Polytype SourceRegion))
    KindOf :: Qualified TypeName -> Query (WithRock (ConsQueryEffects '[])) (Maybe KindVar)
    -- | Get Information about a type alias
    GetTypeAlias ::
        -- | The name of the type alias
        Qualified TypeName ->
        {- | The type alias's type variables and body, if it exists
        \* To Core Queries
        -}
        Query (WithRock (ConsQueryEffects '[])) (Maybe ([UniqueTyVar], Infer.Type SourceRegion))
    -- \* Core Queries
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

    -- | Core → JVM IR (per module)
    GetJVMIRModule :: ModuleName -> Query (WithRock (ConsQueryEffects '[Error JVMLoweringError])) IR.Module
    -- | JVM IR → ClassFiles (per module)
    GetJVMClassFiles :: ModuleName -> Query (WithRock (ConsQueryEffects '[Error JVMLoweringError])) [ClassFile]
    -- | ClassFiles → Serialized bytes (per module)
    GetJVMClassBytes :: ModuleName -> Query (WithRock (ConsQueryEffects '[Error JVMLoweringError, Error CodeConverterError])) [(FilePath, LByteString)]

-- | List of query kinds at the type level
type data QueryType
    = QueryRequiredDeclarationByName
    | QueryDeclarationByName
    | QueryModuleByName
    | QueryConstructorDeclaration
    | DeclarationAnnotations
    | DeclarationAnnotationsOfType

type family QueryReturnTypeOf (q :: QueryType) ast = r where
    QueryReturnTypeOf QueryDeclarationByName ast = Maybe (New.Declaration SourceRegion ast)
    QueryReturnTypeOf QueryRequiredDeclarationByName ast = New.Declaration SourceRegion ast
    QueryReturnTypeOf QueryModuleByName ast = NewModule.Module SourceRegion ast
    QueryReturnTypeOf QueryConstructorDeclaration ast = New.Declaration SourceRegion ast
    QueryReturnTypeOf DeclarationAnnotations ast = [New.Annotation SourceRegion ast]
    QueryReturnTypeOf DeclarationAnnotationsOfType ast = [New.Annotation SourceRegion ast]

type family QueryArgsOf (q :: QueryType) ast where
    QueryArgsOf QueryDeclarationByName ast = Qualified Name
    QueryArgsOf QueryRequiredDeclarationByName ast = Qualified Name
    QueryArgsOf QueryModuleByName ast = ModuleName
    QueryArgsOf QueryConstructorDeclaration ast = ConstructorOccurrence ast SourceRegion
    QueryArgsOf DeclarationAnnotations ast = Qualified Name
    -- \| Args are (declaration name, type name)
    QueryArgsOf DeclarationAnnotationsOfType ast =
        ( Qualified Name
        , Qualified TypeName
        )

class
    ( Typeable ast
    , HasMinimumQueryEffects (QueryEffectsOf q ast)
    ) =>
    SupportsQuery (q :: QueryType) (ast :: Kind.Type)
    where
    {- | Effects that are "unique" to this query.
    Effects included in 'MinimumQueryEffects' should not be included here as they are expected to always be present
    -}
    type QuerySpecificEffectsOf q ast :: [Effect]

    type QuerySpecificEffectsOf q ast = StandardQueryError ast

    query :: HasCallStack => QueryArgsOf q ast -> Eff (QueryEffectsOf q ast) (QueryReturnTypeOf q ast)

type family SupportsQueries (qs :: [QueryType]) ast = (c :: Constraint) where
    SupportsQueries '[] ast = ()
    SupportsQueries (q ': qs) ast =
        ( SupportsQuery q ast
        , SupportsQueries qs ast
        )

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
