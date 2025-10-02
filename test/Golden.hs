module Golden where

import Boilerplate ( finaliseEffects, pipelineResShouldSucceed)
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem (runFileSystem)
import Elara.Data.Unique.Effect (uniqueGenToGlobalIO)
import Elara.Interpreter qualified as Interpreter
import Elara.Logging (ignoreStructuredDebug)
import Elara.Query qualified
import Elara.Rules qualified
import Elara.Settings (CompilerSettings (..), RunWithOption (..), defaultDumpSettings)
import Rock qualified
import Rock.Memo qualified
import Rock.MemoE (memoiseRunIO)
import System.IO.Silently (capture_)
import Test.Syd (Spec, describe, goldenStringFile, it)

spec :: Spec
spec = describe "Golden tests" $ do
    it "Runs hello world" $ do
        let compilerSettings =
                CompilerSettings
                    { mainFile = Just "test/test_resources/golden_inputs/simple-1.txt"
                    , dumpSettings = defaultDumpSettings
                    , runWith = RunWithInterpreter
                    }

        goldenStringFile "test/test_resources/golden_outputs/simple-1.txt" $ do
                capture_ $
                    pipelineResShouldSucceed $
                        finaliseEffects $
                            runFileSystem $
                                uniqueGenToGlobalIO $
                                    ignoreStructuredDebug $
                                        runConcurrent $
                                            memoiseRunIO @Elara.Query.Query $
                                                Rock.runRock (Rock.Memo.memoise (Elara.Rules.rules compilerSettings)) $
                                                    Interpreter.runInterpreter Interpreter.run
