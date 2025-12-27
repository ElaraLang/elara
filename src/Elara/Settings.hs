module Elara.Settings where

data CompilerSettings = CompilerSettings
    { dumpSettings :: DumpSettings
    , runWith :: RunWithOption
    , mainFile :: Maybe FilePath
    }

data DumpSettings = DumpSettings
    { dumpLexed :: Bool
    , dumpParsed :: Bool
    , dumpDesugared :: Bool
    , dumpRenamed :: Bool
    , dumpShunted :: Bool
    , dumpTyped :: Bool
    , dumpCore :: Bool
    }

defaultDumpSettings :: DumpSettings
defaultDumpSettings =
    DumpSettings
        { dumpLexed = False
        , dumpParsed = False
        , dumpDesugared = False
        , dumpRenamed = False
        , dumpShunted = False
        , dumpTyped = False
        , dumpCore = False
        }

data RunWithOption
    = RunWithNone
    | RunWithInterpreter
    | RunWithJVM
    deriving (Eq, Show)
