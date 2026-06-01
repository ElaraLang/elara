-- | Golden tests for end-to-end compiler validation
module Golden (spec) where

import Colog.Core (LogAction (..))
import Data.Text (stripEnd)
import Effectful (Eff)
import Effectful.Colog (runLogAction)
import Effectful.State.Static.Local (execState, modify)
import Error.Diagnose (TabSize (..), WithUnicode (..), prettyDiagnostic')
import Prettyprinter (defaultLayoutOptions, layoutSmart)
import System.Process (readProcess)
import Test.Syd (GoldenTest, Spec, describe, expectationFailure, goldenStringFile, it)

import Prettyprinter.Render.Text qualified as Text

import Boilerplate (finaliseEffects, pipelineResShouldSucceed)
import Elara.Data.Pretty (AnsiStyle, Doc)
import Elara.Error.Diagnose
import Elara.Settings (CompilerSettings (..), defaultSettings)

import Elara qualified
import Elara.Interpreter qualified as Interpreter

spec :: Spec
spec = describe "Golden tests" $ do
    let backends = [Elara.Interpreter, Elara.JVM]
    forM_ backends $ \backend -> describe (show backend) $ do
        it "Runs hello world" $ do
            runGolden defaultSettings backend "simple-1"

        it "Counts to ten" $
            runGolden defaultSettings backend "count-to-ten"

        it "Computes Fibonacci recursively" $
            runGolden defaultSettings backend "recursion"

        it "Currying and partial application" $
            runGolden defaultSettings backend "currying"

        describe "Tuples" $ do
            it "Tuples and pattern matching on tuples" $
                runGolden defaultSettings backend "tuples"

            it "Larger tuples" $
                runGolden defaultSettings backend "bigger-tuples"

        it "Type aliases" $
            runGolden defaultSettings backend "type-aliases"

        it "ADTs with pattern matching" $
            runGolden defaultSettings backend "adt-shapes"

        it "Option type and matching" $
            runGolden defaultSettings backend "option-type"

        it "Higher-order functions (identity, const, flip)" $
            runGolden defaultSettings backend "higher-order"

        it "List operations (map, filter, lambdas)" $
            runGolden defaultSettings backend "list-operations"

        it "Closures capturing variables" $
            runGolden defaultSettings backend "closures"

        it "Nested pattern matching on recursive ADT" $
            runGolden defaultSettings backend "nested-patterns"

        it "Result type with qualified imports" $
            runGolden defaultSettings backend "result-type"

        it "String operations" $
            runGolden defaultSettings backend "string-ops"

        it "IO sequencing with >>" $
            runGolden defaultSettings backend "io-sequencing"

    -- Note: let-binding shadowing (rebinding x after defining x) crashes with
    -- UnknownVariable. This is a known bug tracked separately.

    -- failing tests fail before codegen anyway, so no point running them across multiple backends
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

        it "If expression without else branch" $
            runGoldenError defaultSettings "error-if-no-else"

        it "Unclosed list literal" $
            runGoldenError defaultSettings "error-unclosed-list"

        it "Trailing operator with no right operand" $
            runGoldenError defaultSettings "error-trailing-operator"

        describe "Rename error golden tests" $ do
            it "Explicit import of a name that doesn't exist in the module" $
                runGoldenError defaultSettings "error-rename-nonexistent-import"

            it "Type alias body references an unbound type variable" $
                runGoldenError defaultSettings "error-rename-unknown-type-var"

            it "Block whose last statement is a let binding" $
                runGoldenError defaultSettings "error-rename-block-ends-with-let"

            it "Type alias that refers to itself" $
                runGoldenError defaultSettings "error-rename-recursive-alias"

runGolden :: CompilerSettings -> Elara.Backend -> FilePath -> GoldenTest String
runGolden settings backend goldenName = do
    let inputPrefix = "test/test_resources/golden_inputs/"
    let compilerSettings =
            settings
                { mainFile = Just (inputPrefix <> goldenName <> ".elr")
                , outputDir = "test/build/" <> show backend <> "/" <> goldenName
                }
    goldenStringFile ("test/test_resources/golden_outputs/" <> goldenName <> ".txt") $ do
        case backend of
            Elara.Interpreter -> runInterpreterCapture compilerSettings
            Elara.JVM -> runJVMCapture compilerSettings

runInterpreterCapture :: CompilerSettings -> IO String
runInterpreterCapture compilerSettings = do
    output <-
        pipelineResShouldSucceed $
            finaliseEffects $
                runLogAction (LogAction (const pass) :: LogAction (Eff _) (Doc AnsiStyle)) $
                    Elara.withCompilerEnv compilerSettings $
                        execState ([] :: [Text]) $
                            Interpreter.interpretInterpreterOutput (modify . (:)) $
                                Interpreter.runInterpreter Interpreter.run

    pure (toString $ stripEnd $ unlines $ reverse output)

runJVMCapture :: CompilerSettings -> IO String
runJVMCapture compilerSettings = do
    -- 1. Run the compiler to emit the .class files
    _ <-
        pipelineResShouldSucceed $
            finaliseEffects $
                runLogAction (LogAction (const pass) :: LogAction (Eff _) (Doc AnsiStyle)) $
                    Elara.withCompilerEnv compilerSettings $
                        Elara.compile compilerSettings (Elara.CompileAndEmit Elara.JVM)

    -- 2. Execute the generated bytecode and capture stdout
    let classPath = compilerSettings.outputDir <> ":jvm-stdlib"

    -- readProcess intercepts stdout instead of printing it to the terminal
    rawOutput <- readProcess "java" ["-cp", classPath, "Main"] ""

    -- Normalise trailing newlines
    pure (toString $ stripEnd $ fromString rawOutput)

-- | Run a golden test that expects compiler failure, comparing the diagnostic output
runGoldenError :: CompilerSettings -> FilePath -> GoldenTest String
runGoldenError settings goldenName = do
    let inputPrefix = "test/test_resources/golden_inputs/"
    let compilerSettings =
            settings
                { mainFile = Just (inputPrefix <> goldenName <> ".elr")
                }
    goldenStringFile ("test/test_resources/golden_outputs/" <> goldenName <> ".txt") $ do
        (reports, _result) <-
            finaliseEffects $
                runLogAction (LogAction (const pass) :: LogAction (Eff _) (Doc AnsiStyle)) $
                    Elara.withCompilerEnv compilerSettings $
                        execState ([] :: [Text]) $
                            Interpreter.interpretInterpreterOutput (modify . (:)) $
                                Interpreter.runInterpreter Interpreter.run
        when (null reports) $
            expectationFailure "Expected compiler error but compilation succeeded"
        diag <- reportsToDiagnostic reports
        let rendered = prettyDiagnostic' WithUnicode (TabSize 4) diag
        let plainText = Text.renderStrict $ layoutSmart defaultLayoutOptions rendered
        pure (toString plainText)
