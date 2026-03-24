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
import Error.Diagnose (TabSize (..), WithUnicode (..), hasReports, prettyDiagnostic')
import Prettyprinter (defaultLayoutOptions, layoutSmart)
import Prettyprinter.Render.Text qualified as Text
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

    it "ADTs with pattern matching" $
        runGolden defaultSettings "adt-shapes"

    it "Option type and matching" $
        runGolden defaultSettings "option-type"

    it "Higher-order functions (identity, const, flip)" $
        runGolden defaultSettings "higher-order"

    it "List operations (map, filter, lambdas)" $
        runGolden defaultSettings "list-operations"

    it "Closures capturing variables" $
        runGolden defaultSettings "closures"

    it "Nested pattern matching on recursive ADT" $
        runGolden defaultSettings "nested-patterns"

    it "Result type with qualified imports" $
        runGolden defaultSettings "result-type"

    it "String operations" $
        runGolden defaultSettings "string-ops"

    it "IO sequencing with >>" $
        runGolden defaultSettings "io-sequencing"

    -- Note: let-binding shadowing (rebinding x after defining x) crashes with
    -- UnknownVariable. This is a known bug tracked separately.

    describe "Error golden tests" $ do
        it "Type mismatch in function argument" $
            runGoldenError defaultSettings "error-type-mismatch"

        it "Undefined variable reference" $
            runGoldenError defaultSettings "error-undefined-var"

        it "Undefined type in annotation" $
            runGoldenError defaultSettings "error-undefined-type"

        it "Parse error surfaces as module not found" $
            runGoldenError defaultSettings "error-parse-missing-expr"

        it "Duplicate definition" $
            runGoldenError defaultSettings "error-duplicate-def"

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

-- | Run a golden test that expects compiler failure, comparing the diagnostic output
runGoldenError :: CompilerSettings -> FilePath -> GoldenTest String
runGoldenError settings goldenName = do
    let inputPrefix = "test/test_resources/golden_inputs/"
    let compilerSettings =
            settings
                { mainFile = Just (inputPrefix <> goldenName <> ".elr")
                }
    goldenStringFile ("test/test_resources/golden_outputs/" <> goldenName <> ".txt") $ do
        (diagnostics, _result) <-
            finaliseEffects $
                runLogAction (LogAction (const pass) :: LogAction (Eff _) (Doc AnsiStyle)) $
                    Elara.withCompilerEnv compilerSettings $
                        execState ([] :: [Text]) $
                            Interpreter.interpretInterpreterOutput (modify . (:)) $
                                Interpreter.runInterpreter Interpreter.run
        unless (hasReports diagnostics) $
            error "Expected compiler error but compilation succeeded"
        let rendered = prettyDiagnostic' WithUnicode (TabSize 4) diagnostics
        let plainText = Text.renderStrict $ layoutSmart defaultLayoutOptions rendered
        pure (toString plainText)
