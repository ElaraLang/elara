{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use id" #-}
-- for the HasMemoiseE instance

module Elara.Query where

import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Hashable (hash)
import Effectful
import Effectful.FileSystem (FileSystem)

import Data.Graph (SCC)
import Effectful.Error.Static (Error)
import Elara.AST.Generic (Declaration)
import Elara.AST.Module
import Elara.AST.Name (ModuleName, Name, Qualified, TypeName, VarName)
import Elara.AST.Region (SourceRegion)
import Elara.AST.Select
import Elara.AST.VarRef (IgnoreLocVarRef)
import Elara.Data.Kind (KindVar)
import Elara.Desugar.Error (DesugarError)
import Elara.Lexer.Token
import Elara.Lexer.Utils (LexerError)
import Elara.Parse.Error (ElaraParseError, WParseErrorBundle)
import Elara.Parse.Stream (TokenStream)
import Elara.Query.Effects
import Elara.ReadFile (FileContents)
import Elara.Rename.Error (RenameError)
import Elara.SCC.Type (ReachableSubgraph, SCCKey)
import Elara.Settings (CompilerSettings)
import Elara.Shunt.Error (ShuntError)
import Elara.Shunt.Operator (OpInfo, OpTable)
import Elara.TypeInfer.Environment (TypeEnvKey)
import Elara.TypeInfer.Type (Polytype, Type)
import Rock (Rock)
import Rock.Memo (HasMemoiseE (..))

type WithRock effects =
    Rock.Rock Elara.Query.Query ': effects

data Query (es :: [Effect]) a where
    GetCompilerSettings :: Query (WithRock MinimumQueryEffects) CompilerSettings
    -- | Query to get all the required input files to be passed to the compiler
    InputFiles :: Query (WithRock (ConsMinimumQueryEffects '[FileSystem])) (HashSet FilePath)
    -- | Query to get the contents of a specific file
    GetFileContents :: FilePath -> Query (WithRock (ConsMinimumQueryEffects '[FileSystem])) FileContents
    -- | Query to get the lexed tokens of a specific file
    LexedFile :: FilePath -> Query (WithRock (ConsQueryEffects '[Error LexerError])) [Lexeme]
    -- | Query to get the parsed module from a file's contents and lexed tokens
    ParsedFile :: FilePath -> Query (WithRock (ConsQueryEffects '[Error (WParseErrorBundle TokenStream ElaraParseError)])) (Module 'Frontend)
    -- | Query to get the file path of a module
    ModulePath :: ModuleName -> Query (WithRock (ConsMinimumQueryEffects '[Rock Query, FileSystem])) FilePath
    -- | Query to get a parsed module by module name
    ParsedModule ::
        ModuleName ->
        Query
            (WithRock (ConsQueryEffects '[Error (WParseErrorBundle TokenStream ElaraParseError)]))
            (Module 'Frontend)
    DesugaredModule ::
        ModuleName ->
        Query (WithRock (ConsQueryEffects '[Error DesugarError])) (Module 'Desugared)
    RenamedModule :: ModuleName -> Query (WithRock (ConsQueryEffects '[Error RenameError])) (Module 'Renamed)
    ShuntedModule :: ModuleName -> Query (WithRock (ConsQueryEffects '[Error ShuntError])) (Module 'Shunted)
    ShuntedDeclarationByName :: Qualified Name -> Query (WithRock (ConsQueryEffects '[Error ShuntError])) (Declaration 'Shunted)
    GetOpInfo :: IgnoreLocVarRef Name -> Query (WithRock (ConsQueryEffects '[])) (Maybe OpInfo)
    GetOpTableIn :: ModuleName -> Query (WithRock (ConsQueryEffects '[])) OpTable
    FreeVarsOf :: Qualified VarName -> Query (WithRock (ConsQueryEffects '[])) (HashSet (Qualified VarName))
    ReachableSubgraphOf :: Qualified VarName -> Query (WithRock (ConsQueryEffects '[])) ReachableSubgraph
    GetSCCsOf :: Qualified VarName -> Query (WithRock (ConsQueryEffects '[])) [SCC (Qualified VarName)]
    SCCKeyOf :: Qualified VarName -> Query (WithRock (ConsQueryEffects '[])) SCCKey
    TypeCheckedModule :: ModuleName -> Query (WithRock (ConsQueryEffects '[])) (Module 'Typed)
    TypeOf :: TypeEnvKey -> Query (WithRock (ConsQueryEffects '[])) (Type SourceRegion)
    InferSCC :: SCCKey -> Query (WithRock (ConsQueryEffects '[])) (Map (Qualified VarName) (Polytype SourceRegion))
    KindOf :: Qualified TypeName -> Query (WithRock (ConsQueryEffects '[])) (Maybe KindVar)

deriving instance Eq (Query es a)

deriving instance Show (Query es a)

deriveGEq ''Query
deriveGShow ''Query

instance Hashable (Query es a) where
    hashWithSalt salt = \case
        GetCompilerSettings -> h 0 ()
        InputFiles -> h 1 ()
        GetFileContents fp -> h 2 fp
        LexedFile fp -> h 3 fp
        ParsedFile fp -> h 4 fp
        ModulePath mn -> h 5 mn
        ParsedModule mn -> h 6 mn
        DesugaredModule mn -> h 7 mn
        RenamedModule mn -> h 8 mn
        ShuntedModule mn -> h 9 mn
        ShuntedDeclarationByName qn -> h 14 qn
        GetOpInfo name -> h 10 name
        GetOpTableIn mn -> h 11 mn
        FreeVarsOf v -> h 15 v
        ReachableSubgraphOf v -> h 16 v
        GetSCCsOf v -> h 17 v
        SCCKeyOf v -> h 19 v
        TypeCheckedModule mn -> h 12 mn
        TypeOf loc -> h 13 loc
        InferSCC key -> h 18 key
        KindOf qtn -> h 20 qtn
      where
        h :: Hashable b => Int -> b -> Int
        h tag payload =
            hash tag `hashWithSalt` payload `hashWithSalt` salt

-- alas, this sucks
instance HasMemoiseE Query where
    withMemoiseE = \case
        GetCompilerSettings -> \x -> x
        InputFiles -> \x -> x
        GetFileContents{} -> \x -> x
        LexedFile{} -> \x -> x
        ParsedFile{} -> \x -> x
        ModulePath{} -> \x -> x
        ParsedModule{} -> \x -> x
        DesugaredModule{} -> \x -> x
        RenamedModule{} -> \x -> x
        ShuntedModule{} -> \x -> x
        ShuntedDeclarationByName{} -> \x -> x
        GetOpInfo{} -> \x -> x
        GetOpTableIn{} -> \x -> x
        FreeVarsOf{} -> \x -> x
        ReachableSubgraphOf{} -> \x -> x
        GetSCCsOf{} -> \x -> x
        SCCKeyOf{} -> \x -> x
        TypeCheckedModule{} -> \x -> x
        TypeOf{} -> \x -> x
        InferSCC{} -> \x -> x
        KindOf{} -> \x -> x
