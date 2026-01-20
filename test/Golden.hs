-- | Golden tests for end-to-end compiler validation
module Golden (spec) where

import Boilerplate (finaliseEffects, pipelineResShouldSucceed)
import Data.Dependent.HashMap qualified as DHashMap
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem (runFileSystem)
import Effectful.State.Static.Local (execState, modify)
import Elara.Data.Unique.Effect (uniqueGenToGlobalIO)
import Elara.Interpreter qualified as Interpreter
import Elara.Logging (ignoreStructuredDebug)
import Elara.Rules qualified
import Elara.Settings (CompilerSettings (..), defaultSettings)
import Rock qualified
import Rock.Memo qualified
import Test.Syd (GoldenTest, Spec, describe, goldenStringFile, it)

spec :: Spec
spec = describe "Golden tests" $ do
    it "Runs hello world" $ do
        runGolden defaultSettings "simple-1"

    it "Counts to ten" $
        runGolden defaultSettings "count-to-ten"

runGolden :: CompilerSettings -> FilePath -> GoldenTest String
runGolden settings goldenName = do
    let inputPrefix = "test/test_resources/golden_inputs/"
    let compilerSettings =
            settings
                { mainFile = Just (inputPrefix <> goldenName <> ".elr")
                }
    goldenStringFile ("test/test_resources/golden_outputs/" <> goldenName <> ".txt") $ do
        startedVar <- newIORef DHashMap.empty
        depsVar <- newIORef mempty
        output <-
            pipelineResShouldSucceed $
                finaliseEffects $
                    runFileSystem $
                        uniqueGenToGlobalIO $
                            ignoreStructuredDebug $
                                runConcurrent $
                                    execState ([] :: [Text]) $
                                        Interpreter.interpretInterpreterOutput (modify . (:)) $
                                            Rock.runRock (Rock.Memo.memoiseWithCycleDetection startedVar depsVar (Elara.Rules.rules compilerSettings)) $ do
                                                Interpreter.runInterpreter Interpreter.run
        pure (toString $ unlines $ reverse output)
