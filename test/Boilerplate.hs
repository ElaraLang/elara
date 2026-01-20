{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Test boilerplate and utilities for running the compiler pipeline in tests
module Boilerplate (
    -- * Expression Loading
    loadRenamedExpr',
    loadRenamedExprIO,
    loadShuntedExpr',
    loadShuntedExprIO,

    -- * Test Utilities
    runQueryEffects,
    finaliseEffects,
    pipelineResShouldSucceed,
    evalPipelineRes,

    -- * Test Data
    fakeTypeEnvironment,
    fakeOperatorTable,
    operatorRenameState,

    -- * Pattern Matching Helpers
    ensureExpressionMatches,
    shouldMatch,
) where

import Common (diagShouldSucceed)
import Control.Exception (throwIO)
import Data.Dependent.HashMap qualified as DHashMap
import Data.Map qualified as Map
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static (Error, runError, throwError)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Local (evalState)
import Effectful.Writer.Static.Local qualified as Eff
import Elara.AST.Generic hiding (TypeVar)
import Elara.AST.Module
import Elara.AST.Name hiding (Name)
import Elara.AST.Region
import Elara.AST.Select
import Elara.AST.VarRef
import Elara.Data.Pretty (AnsiStyle, Doc, prettyToText)
import Elara.Data.Unique.Effect (UniqueGen, uniqueGenToGlobalIO)
import Elara.Desugar
import Elara.Desugar.Error (DesugarError)
import Elara.Error
import Elara.Lexer.Reader
import Elara.Lexer.Token qualified
import Elara.Lexer.Utils (LexerError)
import Elara.Logging (StructuredDebug, ignoreStructuredDebug)
import Elara.Parse.Error (ElaraParseError, WParseErrorBundle (..))
import Elara.Parse.Expression
import Elara.Parse.Primitives (Parser)
import Elara.Parse.Stream (TokenStream (..))
import Elara.Prim (boolName, intName, mkPrimQual, primModuleName)
import Elara.Prim.Rename
import Elara.Query qualified
import Elara.ReadFile
import Elara.Rename (renameExpr)
import Elara.Rename.Error
import Elara.Rename.State (RenameState (..))
import Elara.Shunt (OpLookup, fixExpr)
import Elara.Shunt.Error (ShuntError, ShuntWarning)
import Elara.Shunt.Operator
import Elara.TypeInfer.Environment
import Elara.TypeInfer.Type
import Error.Diagnose (Report (..))
import Error.Diagnose.Diagnostic
import Hedgehog
import Hedgehog.Internal.Property (failDiff, failWith)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift, Name (..), NameFlavour (..))
import Print (printPretty)
import Region (qualifiedTest, testLocated, testRegion)
import Rock qualified
import Rock.Memo qualified
import Test.Syd (expectationFailure)
import Test.Syd.Run (mkNotEqualButShouldHaveBeenEqual)
import Text.Megaparsec (runParserT)
import Text.Show

--------------------------------------------------------------------------------
-- Expression Loading
--------------------------------------------------------------------------------

-- | Load and rename an expression from source text
loadRenamedExpr' ::
    forall w.
    ( DiagnosticWriter (Doc AnsiStyle) :> w
    , IOE :> w
    , Error SomeReportableError :> w
    ) =>
    Text ->
    Eff w (Expr Renamed)
loadRenamedExpr' source = runQueryEffects $ do
    let fp = "<tests>"
    Elara.Error.addFile fp (toString source)
    tokens <- runErrorOrReport @LexerError $ readTokensWith (FileContents fp source)
    parsed <- runErrorOrReport @(WParseErrorBundle _ _) $ do
        parseResult <- inject $ parseExprWith exprParser fp source tokens
        case parseResult of
            Left err -> throwError err
            Right p -> pure p
    desugared <- runErrorOrReport @DesugarError $ evalState mempty $ inject $ desugarExpr parsed

    runReader Nothing $
        runReader (Nothing @(Module Desugared)) $
            evalState operatorRenameState $
                runErrorOrReport @RenameError $
                    renameExpr desugared

-- | Load and rename an expression (IO version)
loadRenamedExprIO :: Text -> IO (Diagnostic (Doc AnsiStyle), Maybe (Expr Renamed))
loadRenamedExprIO source = finaliseEffects $ loadRenamedExpr' source

-- | Load, rename, and shunt an expression using the fake operator table
loadShuntedExpr' ::
    forall w.
    ( DiagnosticWriter (Doc AnsiStyle) :> w
    , IOE :> w
    , Error SomeReportableError :> w
    ) =>
    Text ->
    Eff w (Expr Shunted)
loadShuntedExpr' source = runQueryEffects $ do
    let fp = "<tests>"
    Elara.Error.addFile fp (toString source)
    tokens <- runErrorOrReport @LexerError $ readTokensWith (FileContents fp source)
    parsed <- runErrorOrReport @(WParseErrorBundle _ _) $ do
        parseResult <- inject $ parseExprWith exprParser fp source tokens
        case parseResult of
            Left err -> throwError err
            Right p -> pure p
    desugared <- runErrorOrReport @DesugarError $ evalState mempty $ inject $ desugarExpr parsed

    renamed <-
        runReader Nothing $
            runReader (Nothing @(Module Desugared)) $
                evalState operatorRenameState $
                    runErrorOrReport @RenameError $
                        renameExpr desugared

    -- Shunt the expression using the fake operator table
    runErrorOrReport @ShuntError $
        fmap fst $
            Eff.runWriter @(Set ShuntWarning) $
                let ?lookup = fakeOpLookup
                 in fixExpr renamed

-- | Load and shunt an expression (IO version)
loadShuntedExprIO :: Text -> IO (Diagnostic (Doc AnsiStyle), Maybe (Expr Shunted))
loadShuntedExprIO source = finaliseEffects $ loadShuntedExpr' source

--------------------------------------------------------------------------------
-- Effect Runners
--------------------------------------------------------------------------------

-- | Run effects needed for query-based tests
runQueryEffects ::
    IOE :> r =>
    Eff (Rock.Rock Elara.Query.Query : Concurrent : FileSystem : UniqueGen : StructuredDebug : r) a ->
    Eff r a
runQueryEffects eff = do
    startedVar <- liftIO $ newIORef DHashMap.empty
    depsVar <- liftIO $ newIORef mempty
    ignoreStructuredDebug
        . uniqueGenToGlobalIO
        . runFileSystem
        . runConcurrent
        . Rock.runRock (Rock.Memo.memoiseWithCycleDetection startedVar depsVar testRules)
        $ eff

-- | Finalize effects and collect diagnostics
finaliseEffects ::
    Eff [Error SomeReportableError, DiagnosticWriter (Doc AnsiStyle), IOE] a ->
    IO (Diagnostic (Doc AnsiStyle), Maybe a)
finaliseEffects eff = do
    (diagnostics, r) <- runEff $ runDiagnosticWriter $ do
        result <- runError @SomeReportableError $ eff
        case result of
            Left (callStack, err) -> do
                Elara.Error.report err
                printPretty callStack
                pure Nothing
            Right r -> pure (Just r)
    pure (diagnostics, r)

--------------------------------------------------------------------------------
-- Test Assertions
--------------------------------------------------------------------------------

-- | Assert that a pipeline result succeeds (for sydtest)
pipelineResShouldSucceed :: _ => IO (Diagnostic (Doc AnsiStyle), Maybe b) -> IO b
pipelineResShouldSucceed m = do
    (d, x) <- m
    let hasErrors diag =
            any (\case Err{} -> True; Warn{} -> False) $ reportsOf diag
    when (hasErrors d) $
        expectationFailure $
            toString $
                prettyToText $
                    prettyDiagnostic' WithUnicode (TabSize 4) d
    case x of
        Just ok -> pure ok
        Nothing -> expectationFailure $ toString $ prettyToText $ prettyDiagnostic' WithUnicode (TabSize 4) d

-- | Assert that a pipeline result succeeds (for hedgehog)
evalPipelineRes :: (MonadIO m, MonadTest m) => IO (Diagnostic (Doc AnsiStyle), Maybe b) -> m b
evalPipelineRes m = do
    (d, x) <- liftIO m
    diagShouldSucceed (d, x)
    case x of
        Just ok -> pure ok
        Nothing -> failWith Nothing $ toString $ prettyToText $ prettyDiagnostic' WithUnicode (TabSize 4) d

--------------------------------------------------------------------------------
-- Test Data
--------------------------------------------------------------------------------

-- | Rename state with fake operators for testing
operatorRenameState :: RenameState
operatorRenameState =
    primitiveRenameState
        <> RenameState
            { varNames = fromList $ map mkFakeVarP ["+", "-", "*", "/", "|>", "=="]
            , typeNames = fromList []
            , typeVars = fromList []
            }
  where
    mkFakeVarP name = (OperatorVarName name, one $ mkFakeVar name)

-- | Fake operator table for testing shunting
fakeOperatorTable :: OpTable
fakeOperatorTable =
    fromList
        [ mkFakeVarP "+" (OpInfo (mkPrecedence 6) LeftAssociative)
        , mkFakeVarP "-" (OpInfo (mkPrecedence 6) LeftAssociative)
        , mkFakeVarP "*" (OpInfo (mkPrecedence 7) LeftAssociative)
        , mkFakeVarP "/" (OpInfo (mkPrecedence 7) LeftAssociative)
        , mkFakeVarP "|>" (OpInfo (mkPrecedence 1) RightAssociative)
        , mkFakeVarP "==" (OpInfo (mkPrecedence 4) NonAssociative)
        ]
  where
    mkFakeVarP name info = (ignoreLocation $ withName $ mkFakeOp name, info)

-- | Fake type environment for testing type inference
fakeTypeEnvironment :: TypeEnvironment SourceRegion
fakeTypeEnvironment =
    emptyTypeEnvironment
        & addType (TermVarKey (qualifiedTest $ OperatorVarName "+")) (Lifted intToIntToInt)
        & addType (TermVarKey (qualifiedTest $ OperatorVarName "-")) (Lifted intToIntToInt)
        & addType (TermVarKey (qualifiedTest $ OperatorVarName "*")) (Lifted intToIntToInt)
        & addType (TermVarKey (qualifiedTest $ OperatorVarName "/")) (Lifted intToIntToInt)
        & addType (TermVarKey (qualifiedTest $ OperatorVarName "|>")) (Lifted intToIntToInt)
        & addType (TermVarKey (qualifiedTest $ OperatorVarName "==")) (Lifted intToIntToBool)
        & addType (DataConKey (Qualified "True" primModuleName)) (Lifted boolType)
        & addType (DataConKey (Qualified "False" primModuleName)) (Lifted boolType)
  where
    intType = TypeConstructor testRegion (mkPrimQual intName) []
    boolType = TypeConstructor testRegion (mkPrimQual boolName) []
    intToIntToInt = Function testRegion intType (Function testRegion intType intType)
    intToIntToBool = Function testRegion intType (Function testRegion intType boolType)

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Minimal rules for testing - only handles errors
testRules :: HasCallStack => Rock.Rules Elara.Query.Query
testRules = \case
    _other -> error "No rule for this query in test environment"

-- | Helper to parse an expression for testing
parseExprWith ::
    Parser a ->
    FilePath ->
    Text ->
    [Elara.Lexer.Token.Lexeme] ->
    Eff '[StructuredDebug] (Either (WParseErrorBundle TokenStream ElaraParseError) a)
parseExprWith parser fp source tokens = do
    let tokenStream = TokenStream source tokens False
    first WParseErrorBundle <$> runParserT parser fp tokenStream

-- | Create an OpLookup from the fake operator table
fakeOpLookup :: OpLookup es
fakeOpLookup name = pure $ Map.lookup name fakeOperatorTable

mkFakeVar :: OpName -> VarRef VarName
mkFakeVar name = Global (testLocated (qualifiedTest (OperatorVarName name)))

mkFakeOp :: OpName -> VarRef OpName
mkFakeOp name = Global (testLocated (qualifiedTest name))

--------------------------------------------------------------------------------
-- Pattern Matching Helpers (Template Haskell)
--------------------------------------------------------------------------------

-- | Create a pattern matching assertion for hedgehog
ensureExpressionMatches :: HasCallStack => Q Pat -> Q Exp
ensureExpressionMatches qpat = do
    pat <- qpat
    let msg = Shown $ pprint (stripQualifiers pat)
    [e|\case $(pure pat) -> pass; o -> withFrozenCallStack $ failDiff msg o|]

-- | Create a pattern matching assertion for sydtest
shouldMatch :: HasCallStack => Q Pat -> Q Exp
shouldMatch qpat = do
    pat <- qpat
    let msg = pprint (stripQualifiers pat)
    [|\case $(pure pat) -> pass; o -> throwIO =<< mkNotEqualButShouldHaveBeenEqual (ppShow o) msg|]

newtype Shown = Shown String deriving (Lift)

instance Show Shown where
    show (Shown s) = s

stripQualifiers :: Pat -> Pat
stripQualifiers = transformOf gplate $ \case
    ConP name t ps -> ConP (stripQualifiersName name) t ps
    x -> x
  where
    stripQualifiersName :: Name -> Name
    stripQualifiersName (Name x (NameQ _)) = Name x NameS
    stripQualifiersName (Name x (NameG{})) = Name x NameS
    stripQualifiersName n = n
