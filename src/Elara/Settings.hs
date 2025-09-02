module Elara.Settings where

newtype CompilerSettings = CompilerSettings
    { dumpSettings :: DumpSettings
    }

data DumpSettings = DumpSettings
    { dumpLexed :: Bool
    , dumpParsed :: Bool
    , dumpDesugared :: Bool
    , dumpShunted :: Bool
    , dumpTyped :: Bool
    , dumpCore :: Bool
    }
