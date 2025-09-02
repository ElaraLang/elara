{-# LANGUAGE TemplateHaskell #-}

module Elara.Query where

import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Hashable (hash)
import Effectful
import Effectful.FileSystem (FileSystem)

import Effectful.Error.Static (Error)
import Elara.AST.Module
import Elara.AST.Select
import Elara.Data.Pretty (AnsiStyle, Doc)
import Elara.Error (SomeReportableError)
import Elara.Error.EffectNew
import Elara.Lexer.Token
import Elara.Lexer.Utils (LexerError)
import Elara.Parse.Error (ElaraParseError, WParseErrorBundle)
import Elara.Parse.Stream (TokenStream)
import Elara.ReadFile (FileContents)
import Elara.Settings (CompilerSettings)
import Rock (Rock)

type StandardQueryEffects = ConsQueryEffects '[]

type ConsQueryEffects :: [Effect] -> [Effect]
type ConsQueryEffects es =
    FileSystem
        ': Rock Query
        ': Error SomeReportableError
        ': DiagnosticWriter (Doc AnsiStyle)
        ': es

data Query (es :: [Effect]) a where
    -- | Query to get all the required input files to be passed to the compiler
    InputFiles :: Query '[FileSystem] (HashSet FilePath)
    -- | Query to get the contents of a specific file
    GetFileContents :: FilePath -> Query '[FileSystem] FileContents
    -- | Query to get the lexed tokens of a specific file
    LexedFile :: FilePath -> Query (ConsQueryEffects '[Error LexerError]) [Lexeme]
    -- | Query to get the parsed module from a file's contents and lexed tokens
    ParsedFile :: FilePath -> Query (ConsQueryEffects '[Error (WParseErrorBundle TokenStream ElaraParseError)]) (Module 'Frontend)
    GetCompilerSettings :: Query '[] CompilerSettings

deriving instance Eq (Query es a)

deriving instance Show (Query es a)

deriveGEq ''Query
deriveGCompare ''Query
deriveGShow ''Query

instance Hashable (Query es a) where
    hashWithSalt salt = \case
        InputFiles -> h 0 ()
        GetFileContents fp -> h 1 fp
        LexedFile fp -> h 2 fp
        ParsedFile fp -> h 3 fp
        GetCompilerSettings -> h 4 ()
      where
        h :: Hashable b => Int -> b -> Int
        h tag payload =
            hash tag `hashWithSalt` payload `hashWithSalt` salt
