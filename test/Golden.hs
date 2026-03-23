-- | Golden tests for end-to-end compiler validation
module Golden (spec) where

import Boilerplate (finaliseEffects, pipelineResShouldSucceed)
import Colog.Core (LogAction (..))
import Effectful (Eff)
import Effectful.Colog (runLogAction)
import Effectful.State.Static.Local (execState, modify)
import Elara qualified
import Elara.Data.Pretty (AnsiStyle, Doc)
import Elara.Interpreter qualified as Interpreter
import Elara.Settings (CompilerSettings (..), defaultSettings)
import Test.Syd (GoldenTest, Spec, describe, goldenStringFile, it)

spec :: Spec
spec = describe "Golden tests" $ do
    it "Runs hello world" $ do
        runGolden defaultSettings "simple-1"

    it "Counts to ten" $
        runGolden defaultSettings "count-to-ten"

    it "Computes Fibonacci recursively" $
        runGolden defaultSettings "recursion"

    it "Currying and partial application" $
        runGolden defaultSettings "currying"

    it "Tuples and pattern matching on tuples" $
        runGolden defaultSettings "tuples"

    it "Type aliases" $
        runGolden defaultSettings "type-aliases"

runGolden :: CompilerSettings -> FilePath -> GoldenTest String
runGolden settings goldenName = do
    let inputPrefix = "test/test_resources/golden_inputs/"
    let compilerSettings =
            settings
                { mainFile = Just (inputPrefix <> goldenName <> ".elr")
                }
    goldenStringFile ("test/test_resources/golden_outputs/" <> goldenName <> ".txt") $ do
        output <-
            pipelineResShouldSucceed $
                finaliseEffects $
                    runLogAction (LogAction (const pass) :: LogAction (Eff _) (Doc AnsiStyle)) $
                        Elara.withCompilerEnv compilerSettings $
                            execState ([] :: [Text]) $
                                Interpreter.interpretInterpreterOutput (modify . (:)) $
                                    Interpreter.runInterpreter Interpreter.run
        pure (toString $ unlines $ reverse output)
