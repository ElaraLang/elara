module Elara.Settings where

newtype CompilerSettings = CompilerSettings
    { dumpSettings :: DumpSettings
    }

data DumpSettings = DumpSettings
    { dumpLexed :: Bool
    , dumpParsed :: Bool
    , dumpDesugared :: Bool
    , dumpRenamed :: Bool
    , dumpShunted :: Bool
    , dumpTyped :: Bool
    , dumpCore :: Bool
    , runWith :: RunWithOption
    }

data RunWithOption
    = RunWithNone
    | RunWithInterpreter
    deriving (Eq, Show)
