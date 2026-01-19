module Elara.Settings where

data CompilerSettings = CompilerSettings
    { dumpTargets :: Set DumpTarget
    -- ^ Which compilation stages to dump
    , runWith :: RunWithOption
    -- ^ How to run the compiled program
    , mainFile :: Maybe FilePath
    -- ^ The main file to compile/run
    , sourceDirs :: [FilePath]
    -- ^ Directories to search for source files
    }
    deriving (Show, Eq)

data DumpTarget
    = DumpLexed
    | DumpParsed
    | DumpDesugared
    | DumpRenamed
    | DumpShunted
    | DumpTyped
    | DumpCore
    | DumpIR
    | DumpJVM
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Default is simply an empty set
defaultSettings :: CompilerSettings
defaultSettings =
    CompilerSettings
        { dumpTargets = mempty
        , runWith = RunWithNone
        , mainFile = Nothing
        , sourceDirs = []
        }

data RunWithOption
    = RunWithNone
    | RunWithInterpreter
    | RunWithJVM
    deriving (Eq, Show)
