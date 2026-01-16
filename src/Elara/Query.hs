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

import Data.Hashable (hash)
import Effectful
import Effectful.FileSystem (FileSystem)
import Elara.Query.TH

import Data.Data (type (:~:) (Refl))
import Data.GADT.Compare
import Data.Graph (SCC)
import Data.Kind (Constraint)
import Data.Typeable (eqT, typeRep)
import Effectful.Error.Static (Error)
import Effectful.Writer.Static.Local
import Elara.AST.Generic (Annotation, Declaration, Select)
import Elara.AST.Module
import Elara.AST.Name (ModuleName, Name, Qualified, TypeName, VarName)
import Elara.AST.Region (SourceRegion)
import Elara.AST.Select
import Elara.AST.Typed (TypedExpr)
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
import Elara.Lexer.Token
import Elara.Lexer.Utils (LexerError)
import Elara.Parse.Error (ElaraParseError, WParseErrorBundle)
import Elara.Parse.Stream (TokenStream)
import Elara.Query.Effects
import Elara.Query.Errors
import Elara.ReadFile (FileContents, ReadFileError)
import Elara.Rename.Error (RenameError)
import Elara.SCC.Type (ReachableSubgraph, SCCKey)
import Elara.Settings (CompilerSettings)
import Elara.Shunt.Error (ShuntError, ShuntWarning)
import Elara.Shunt.Operator (OpInfo, OpTable)
import Elara.TypeInfer.Environment (TypeEnvKey)
import Elara.TypeInfer.Type (Polytype, Type)
import Elara.TypeInfer.Unique
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
    -- | Query to get the contents of a specific file
    GetFileContents :: FilePath -> Query (WithRock (ConsMinimumQueryEffects '[FileSystem, Error ReadFileError, DiagnosticWriter (Doc AnsiStyle)])) FileContents
    -- | Query to get the file path of a module
    ModulePath :: ModuleName -> Query (WithRock (ConsMinimumQueryEffects '[Rock Query, FileSystem])) FilePath
    -- \* Lexing and Parsing Queries

    -- | Query to get the lexed tokens of a specific file
    LexedFile :: FilePath -> Query (WithRock (ConsQueryEffects '[Error LexerError])) [Lexeme]
    -- | Query to get the parsed module from a file's contents and lexed tokens
    ParsedFile :: FilePath -> Query (WithRock (ConsQueryEffects '[Error (WParseErrorBundle TokenStream ElaraParseError)])) (Module Frontend)
    -- | Query to get a parsed module by module name
    ParsedModule ::
        ModuleName ->
        Query
            (WithRock (ConsQueryEffects '[Error (WParseErrorBundle TokenStream ElaraParseError)]))
            (Module Frontend)
    -- \* Desugaring and Renaming Queries
    DesugaredModule ::
        ModuleName ->
        Query (WithRock (ConsQueryEffects '[Error DesugarError])) (Module Desugared)
    RenamedModule :: ModuleName -> Query (WithRock (ConsQueryEffects '[Error RenameError])) (Module Renamed)
    -- | Lookup a module by name
    ModuleByName ::
        forall (ast :: LocatedAST).
        ( Typeable ast
        , SupportsQuery QueryModuleByName ast
        , HasMinimumQueryEffects (QueryEffectsOf QueryModuleByName ast)
        ) =>
        ModuleName -> Query (QueryEffectsOf QueryModuleByName ast) (QueryReturnTypeOf QueryModuleByName ast)
    --  | Lookup a declaration by name, which may not exist
    DeclarationByName ::
        forall (ast :: LocatedAST).
        ( Typeable ast
        , SupportsQuery QueryDeclarationByName ast
        ) =>
        QueryArgsOf QueryDeclarationByName ast ->
        Query
            (QueryEffectsOf QueryDeclarationByName ast)
            (QueryReturnTypeOf QueryDeclarationByName ast)
    --  | Lookup a declaration by name, which must exist, throwing an 'InternalError' if not found
    RequiredDeclarationByName ::
        forall (ast :: LocatedAST).
        ( Typeable ast
        , SupportsQuery QueryRequiredDeclarationByName ast
        ) =>
        QueryArgsOf QueryRequiredDeclarationByName ast ->
        Query
            (QueryEffectsOf QueryRequiredDeclarationByName ast)
            (QueryReturnTypeOf QueryRequiredDeclarationByName ast)
    -- | Looks up the declaration for a data constructor
    ConstructorDeclaration ::
        forall (ast :: LocatedAST).
        (Typeable ast, Show (Select ConRef ast), Ord (Select ConRef ast), Hashable (Select ConRef ast), SupportsQuery QueryConstructorDeclaration ast) =>
        QueryArgsOf QueryConstructorDeclaration ast ->
        Query (QueryEffectsOf QueryConstructorDeclaration ast) (QueryReturnTypeOf QueryConstructorDeclaration ast)
    DeclarationAnnotations ::
        forall (ast :: LocatedAST).
        (Typeable ast, SupportsQuery DeclarationAnnotations ast) =>
        QueryArgsOf QueryDeclarationByName ast ->
        Query (QueryEffectsOf DeclarationAnnotations ast) (QueryReturnTypeOf DeclarationAnnotations ast)
    DeclarationAnnotationsOfType ::
        forall (ast :: LocatedAST).
        ( Typeable ast
        , SupportsQuery DeclarationAnnotationsOfType ast
        ) =>
        QueryArgsOf DeclarationAnnotationsOfType ast ->
        Query
            (QueryEffectsOf DeclarationAnnotations ast)
            (QueryReturnTypeOf DeclarationAnnotations ast)
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
    TypeCheckedModule :: ModuleName -> Query (WithRock (ConsQueryEffects '[])) (Module Typed)
    TypeCheckedExpr :: Qualified VarName -> Query (WithRock (ConsQueryEffects '[])) TypedExpr
    TypeOf :: loc ~ SourceRegion => TypeEnvKey loc -> Query (WithRock (ConsQueryEffects '[])) (Type loc)
    InferSCC :: SCCKey -> Query (WithRock (ConsQueryEffects '[])) (Map (Qualified VarName) (Polytype SourceRegion))
    KindOf :: Qualified TypeName -> Query (WithRock (ConsQueryEffects '[])) (Maybe KindVar)
    -- | Get Information about a type alias
    GetTypeAlias ::
        -- | The name of the type alias
        Qualified TypeName ->
        {- | The type alias's type variables and body, if it exists
        \* To Core Queries
        -}
        Query (WithRock (ConsQueryEffects '[])) (Maybe ([UniqueTyVar], Type SourceRegion))
    GetCoreModule :: ModuleName -> Query (WithRock (ConsQueryEffects '[])) (CoreModule CoreBind)
    GetTyCon :: Qualified Text -> Query (WithRock (ConsQueryEffects '[])) (Maybe TyCon)
    GetDataCon :: Qualified TypeName -> Query (WithRock (ConsQueryEffects '[])) (Maybe DataCon)
    -- \* Core To Core
    GetOptimisedCoreModule :: ModuleName -> Query (WithRock (ConsQueryEffects '[])) (CoreModule CoreBind)
    GetANFCoreModule :: ModuleName -> Query (WithRock (ConsQueryEffects '[])) (CoreModule (ANF.TopLevelBind Core.Var))
    GetClosureLiftedModule :: ModuleName -> Query (WithRock (ConsQueryEffects '[Error ClosureLiftError])) (CoreModule (ANF.TopLevelBind Core.Var))
    GetFinalisedCoreModule :: ModuleName -> Query (WithRock (ConsQueryEffects '[])) (CoreModule CoreBind)

-- | List of query kinds at the type level
type data QueryType
    = QueryRequiredDeclarationByName
    | QueryDeclarationByName
    | QueryModuleByName
    | QueryConstructorDeclaration
    | DeclarationAnnotations
    | DeclarationAnnotationsOfType

type family QueryReturnTypeOf (q :: QueryType) ast = r where
    QueryReturnTypeOf QueryDeclarationByName ast = Maybe (Declaration ast)
    QueryReturnTypeOf QueryRequiredDeclarationByName ast = Declaration ast
    QueryReturnTypeOf QueryModuleByName ast = Module ast
    QueryReturnTypeOf QueryConstructorDeclaration ast = Declaration ast
    QueryReturnTypeOf DeclarationAnnotations ast = [Annotation ast]
    QueryReturnTypeOf DeclarationAnnotationsOfType ast = [Annotation ast]

type family QueryArgsOf (q :: QueryType) ast where
    QueryArgsOf QueryDeclarationByName ast = Qualified Name
    QueryArgsOf QueryRequiredDeclarationByName ast = Qualified Name
    QueryArgsOf QueryModuleByName ast = ModuleName
    QueryArgsOf QueryConstructorDeclaration ast = (Select ConRef ast)
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
    SupportsQuery (q :: QueryType) (ast :: LocatedAST)
    where
    {- | Effects that are "unique" to this query.
    Effects included in 'MinimumQueryEffects' should not be included here as they are expected to always be present
    -}
    type QuerySpecificEffectsOf q ast :: [Effect]

    type QuerySpecificEffectsOf q ast = StandardQueryError ast

    query :: HasCallStack => QueryArgsOf q ast -> Eff (QueryEffectsOf q ast) (QueryReturnTypeOf q ast)

type family SupportsQueries (qs :: [QueryType]) (ast :: LocatedAST) = (c :: Constraint) where
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

deriving instance Show (Query es a)

instance GCompare (Query es) => GEq (Query es) where
    geq x y = case gcompare x y of
        GEQ -> Just Refl
        _ -> Nothing

instance GCompare (Query es) where
    gcompare a b =
        case compare (tag a) (tag b) of -- first compare tags (i.e. constructors)
            LT -> GLT
            GT -> GGT
            EQ -> sameCtor a b
      where
        tag = tagQuery
        sameCtor ::
            Query es x -> Query es y -> GOrdering x y
        sameCtor GetCompilerSettings GetCompilerSettings = GEQ
        sameCtor InputFiles InputFiles = GEQ
        sameCtor (GetFileContents p1) (GetFileContents p2) =
            ord p1 p2
        sameCtor (ModulePath m1) (ModulePath m2) =
            ord m1 m2
        sameCtor (LexedFile p1) (LexedFile p2) =
            ord p1 p2
        sameCtor (ParsedFile p1) (ParsedFile p2) =
            ord p1 p2
        sameCtor (ParsedModule m1) (ParsedModule m2) =
            ord m1 m2
        sameCtor (DesugaredModule m1) (DesugaredModule m2) =
            ord m1 m2
        sameCtor (RenamedModule m1) (RenamedModule m2) =
            ord m1 m2
        sameCtor (GetOpInfo v1) (GetOpInfo v2) =
            ord v1 v2
        sameCtor (GetOpTableIn m1) (GetOpTableIn m2) =
            ord m1 m2
        sameCtor (FreeVarsOf v1) (FreeVarsOf v2) =
            ord v1 v2
        sameCtor (ReachableSubgraphOf v1) (ReachableSubgraphOf v2) =
            ord v1 v2
        sameCtor (GetSCCsOf v1) (GetSCCsOf v2) =
            ord v1 v2
        sameCtor (SCCKeyOf v1) (SCCKeyOf v2) =
            ord v1 v2
        sameCtor (TypeCheckedModule m1) (TypeCheckedModule m2) =
            ord m1 m2
        sameCtor (TypeCheckedExpr v1) (TypeCheckedExpr v2) =
            ord v1 v2
        sameCtor (TypeOf k1) (TypeOf k2) =
            ord k1 k2
        sameCtor (InferSCC k1) (InferSCC k2) =
            ord k1 k2
        sameCtor (KindOf t1) (KindOf t2) =
            ord t1 t2
        sameCtor (GetTypeAlias t1) (GetTypeAlias t2) =
            ord t1 t2
        sameCtor (GetCoreModule m1) (GetCoreModule m2) =
            ord m1 m2
        sameCtor (GetTyCon n1) (GetTyCon n2) =
            ord n1 n2
        sameCtor (GetDataCon n1) (GetDataCon n2) =
            ord n1 n2
        sameCtor (GetOptimisedCoreModule m1) (GetOptimisedCoreModule m2) =
            ord m1 m2
        sameCtor (GetANFCoreModule m1) (GetANFCoreModule m2) =
            ord m1 m2
        sameCtor (GetClosureLiftedModule m1) (GetClosureLiftedModule m2) =
            ord m1 m2
        sameCtor (GetFinalisedCoreModule m1) (GetFinalisedCoreModule m2) =
            ord m1 m2
        -- Special: compare both the name and the type index
        sameCtor (DeclarationByName @ast1 n1) (DeclarationByName @ast2 n2) =
            case compare n1 n2 of
                LT -> GLT
                GT -> GGT
                EQ -> case eqT @ast1 @ast2 of
                    Just Refl -> GEQ
                    Nothing ->
                        case compare (typeRep (Proxy @ast1)) (typeRep (Proxy @ast2)) of
                            LT -> GLT
                            GT -> GGT
                            EQ -> GLT -- unreachable, but keep totality
        sameCtor (RequiredDeclarationByName @ast1 n1) (RequiredDeclarationByName @ast2 n2) =
            case compare n1 n2 of
                LT -> GLT
                GT -> GGT
                EQ -> case eqT @ast1 @ast2 of
                    Just Refl -> GEQ
                    Nothing ->
                        case compare (typeRep (Proxy @ast1)) (typeRep (Proxy @ast2)) of
                            LT -> GLT
                            GT -> GGT
                            EQ -> GLT -- unreachable, but keep totality
        sameCtor (ModuleByName @ast1 m1) (ModuleByName @ast2 m2) =
            case compare m1 m2 of
                LT -> GLT
                GT -> GGT
                EQ -> case eqT @ast1 @ast2 of
                    Just Refl -> GEQ
                    Nothing ->
                        case compare (typeRep (Proxy @ast1)) (typeRep (Proxy @ast2)) of
                            LT -> GLT
                            GT -> GGT
                            EQ -> GLT -- unreachable, but keep totality
        sameCtor (ConstructorDeclaration @ast1 c1) (ConstructorDeclaration @ast2 c2) =
            case eqT @ast1 @ast2 of
                Just Refl -> ord c1 c2
                Nothing ->
                    case compare (typeRep (Proxy @ast1)) (typeRep (Proxy @ast2)) of
                        LT -> GLT
                        GT -> GGT
                        EQ -> GLT -- unreachable, but keep totality
        sameCtor (DeclarationAnnotations @ast1 n1) (DeclarationAnnotations @ast2 n2) =
            case compare n1 n2 of
                LT -> GLT
                GT -> GGT
                EQ -> case eqT @ast1 @ast2 of
                    Just Refl -> GEQ
                    Nothing ->
                        case compare (typeRep (Proxy @ast1)) (typeRep (Proxy @ast2)) of
                            LT -> GLT
                            GT -> GGT
                            EQ -> GLT -- unreachable, but keep totality
        sameCtor (DeclarationAnnotationsOfType @ast1 (n1, a1)) (DeclarationAnnotationsOfType @ast2 (n2, a2)) =
            compareThen
                (n1, a1)
                (n2, a2)
                ( case eqT @ast1 @ast2 of
                    Just Refl -> GEQ
                    Nothing ->
                        case compare (typeRep (Proxy @ast1)) (typeRep (Proxy @ast2)) of
                            LT -> GLT
                            GT -> GGT
                            EQ -> GLT -- unreachable, but keep totality
                )

        compareThen a b f = case compare a b of
            LT -> GLT
            GT -> GGT
            EQ -> f
        --  helper for payloads with Ord
        ord :: Ord v => v -> v -> GOrdering a a

        ord x y = case compare x y of
            LT -> GLT
            GT -> GGT
            EQ -> GEQ

-- ordAST :: forall ast1 -> forall ast2 -> GOrdering a a
instance Eq (Query es a) => Hashable (Query es a) where
    hashWithSalt salt q = case q of
        GetCompilerSettings -> h ()
        InputFiles -> h ()
        GetFileContents fp -> h fp
        LexedFile fp -> h fp
        ParsedFile fp -> h fp
        ModulePath mn -> h mn
        ParsedModule mn -> h mn
        DesugaredModule mn -> h mn
        RenamedModule mn -> h mn
        DeclarationByName @ast name -> h (name, typeRep (Proxy @ast))
        RequiredDeclarationByName @ast name -> h (name, typeRep (Proxy @ast))
        ModuleByName @ast mn -> h (mn, typeRep (Proxy @ast))
        ConstructorDeclaration @ast conRef -> h (conRef, typeRep (Proxy @ast))
        DeclarationAnnotations @ast name -> h (name, typeRep (Proxy @ast))
        DeclarationAnnotationsOfType @ast (name, annName) -> h (name, annName, typeRep (Proxy @ast))
        GetOpInfo name -> h name
        GetOpTableIn mn -> h mn
        FreeVarsOf v -> h v
        ReachableSubgraphOf v -> h v
        GetSCCsOf v -> h v
        SCCKeyOf v -> h v
        TypeCheckedModule mn -> h mn
        TypeOf loc -> h loc
        InferSCC key -> h key
        KindOf qtn -> h qtn
        GetTypeAlias qtn -> h qtn
        GetCoreModule mn -> h mn
        GetTyCon qn -> h qn
        GetDataCon qn -> h qn
        GetOptimisedCoreModule mn -> h mn
        GetANFCoreModule mn -> h mn
        GetClosureLiftedModule mn -> h mn
        GetFinalisedCoreModule mn -> h mn
        TypeCheckedExpr qn -> h qn
      where
        t = tagQuery q
        h :: forall b. Hashable b => b -> Int
        h payload = hash t `hashWithSalt` payload `hashWithSalt` salt
