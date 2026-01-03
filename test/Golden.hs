module Golden where

import Boilerplate (finaliseEffects, pipelineResShouldSucceed)
import Effectful.Concurrent (runConcurrent, threadDelay)
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
import Test.Syd (GoldenTest, Spec, describe, goldenStringFile, it, sequential)

defaultSettings =
    CompilerSettings
        { dumpSettings = defaultDumpSettings
        , runWith = RunWithNone
        , mainFile = Nothing
        }

spec :: Spec
spec = describe "Golden tests" $ sequential $ do
    it "Runs hello world" $ do
        runGolden defaultSettings "simple-1"

    it
        "Counts to ten"
        (runGolden defaultSettings "count-to-ten")

runGolden :: CompilerSettings -> FilePath -> GoldenTest String
runGolden settings goldenName = do
    let inputPrefix = "test/test_resources/golden_inputs/"
    let compilerSettings =
            settings
                { mainFile = Just (inputPrefix <> goldenName <> ".elr")
                }
    goldenStringFile ("test/test_resources/golden_outputs/" <> goldenName <> ".txt") $ do
        capture_ $
            pipelineResShouldSucceed $
                finaliseEffects $
                    runFileSystem $
                        uniqueGenToGlobalIO $
                            ignoreStructuredDebug $
                                runConcurrent $
                                    memoiseRunIO @Elara.Query.Query $
                                        Rock.runRock (Rock.Memo.memoise (Elara.Rules.rules compilerSettings)) $ do
                                            -- the capture_ function is not thread safe and so can sometimes
                                            -- intercept the output from the test runner
                                            -- adding a small delay here seems to mostly mitigate the issue
                                            -- albeit in a stupid way
                                            threadDelay 5000
                                            Interpreter.runInterpreter Interpreter.run
