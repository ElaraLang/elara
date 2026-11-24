{-# LANGUAGE ImplicitParams #-}

-- | Test boilerplate and utilities for running the compiler pipeline in tests
module Boilerplate (
    -- * Expression Loading
    loadShuntedExpr',
    loadShuntedExprIO,

    -- * Test Utilities
    runQueryEffects,
    finaliseEffects,
    pipelineResShouldSucceed,

    -- * Test Data
    fakeTypeEnvironment,
    fakeOperatorTable,
    operatorRenameState,

    -- * Pattern Matching Helpers
)
where

import Effectful
import Effectful.Error.Static (Error, runError, throwError)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Local (evalState)
import Error.Diagnose.Diagnostic
import Language.Haskell.TH.Syntax (Lift)
import Test.Syd (expectationFailure)
import Text.Megaparsec (runParserT)
import Text.Show

import Data.Dependent.HashMap qualified as DHashMap
import Data.Map qualified as Map
import Effectful.Writer.Static.Local qualified as Eff

import Elara.AST.Name hiding (Name)
import Elara.AST.Region
import Elara.AST.VarRef
import Elara.Data.Pretty (prettyToText)
import Elara.Data.Unique.Effect (UniqueGen, uniqueGenToGlobalIO)
import Elara.Desugar (DesugarState, desugarExpr)
import Elara.Desugar.Error (DesugarError)
import Elara.Error
import Elara.Error.Diagnose
import Elara.Lexer.Reader
import Elara.Lexer.Utils (LexerError)
import Elara.Logging (StructuredDebug, ignoreStructuredDebug)
import Elara.Parse.Error (ElaraParseError, WParseErrorBundle (..))
import Elara.Parse.Grammar
import Elara.Parse.Primitives (Parser)
import Elara.Parse.Stream (TokenStream (..))
import Elara.Prim (KnownType (..), KnownTypeInfo (..), OpaquePrim (..), WiredInPrim (..), knownTypeInfo, primModuleName)
import Elara.Prim.Rename
import Elara.ReadFile
import Elara.Rename (renameExpr)
import Elara.Rename.Error
import Elara.Rename.State (RenameState (..))
import Elara.Shunt (OpLookup, fixExpr)
import Elara.Shunt.Error (ShuntError, ShuntWarning)
import Elara.Shunt.Operator
import Elara.TypeInfer.Environment
import Elara.TypeInfer.Type
import Region (qualifiedTest, testLocated, testRegion)

import Elara.AST.Module qualified as NewModule
import Elara.AST.Phases.Desugared qualified as NewD
import Elara.AST.Phases.Shunted qualified as NewS
import Elara.Lexer.Token qualified
import Elara.Query qualified
import Rock qualified
import Rock.Memo qualified

-- | Load, rename, and shunt an expression using the fake operator table
loadShuntedExpr' ::
    forall w.
    ( IOE :> w
    , Error ElaraError :> w
    , Eff.Writer [ElaraWarning] :> w
    ) =>
    Text ->
    Eff w NewS.ShuntedExpr
loadShuntedExpr' source = runQueryEffects $ do
    let fp = "<tests>"
    tokens <- runErrorAsElaraError @LexerError $ readTokensWith (FileContents fp source)
    parsed <- runErrorAsElaraError @(WParseErrorBundle TokenStream ElaraParseError) $ do
        parseResult <- inject $ parseExprWith exprParser fp source tokens
        case parseResult of
            Left err -> throwError err
            Right p -> pure p
    newDesugared <- runErrorAsElaraError @DesugarError $ evalState @DesugarState mempty $ inject $ desugarExpr parsed

    newRenamed <-
        runReader Nothing $
            runReader (Nothing @(NewModule.Module SourceRegion NewD.Desugared)) $
                evalState operatorRenameState $
                    runErrorAsElaraError @RenameError $
                        renameExpr newDesugared

    -- Shunt the expression using the fake operator table
    runErrorAsElaraError @ShuntError $
        runWriterAsElaraWarning @ShuntWarning @_ @_ @Set $
            let ?lookup = fakeOpLookup
             in fixExpr newRenamed

-- | Load and shunt an expression and run all the effects
loadShuntedExprIO :: Text -> IO ([ElaraReport], Maybe NewS.ShuntedExpr)
loadShuntedExprIO source = finaliseEffects $ loadShuntedExpr' source

-- | Run effects needed for query-based tests
runQueryEffects ::
    IOE :> r =>
    Eff (Rock.Rock Elara.Query.Query : FileSystem : UniqueGen : StructuredDebug : r) a ->
    Eff r a
runQueryEffects eff = do
    startedVar <- liftIO $ newMVar DHashMap.empty
    depsVar <- liftIO $ newIORef mempty
    ignoreStructuredDebug
        . uniqueGenToGlobalIO
        . runFileSystem
        . Rock.runRock (Rock.Memo.memoiseWithCycleDetection startedVar depsVar testRules)
        $ eff

-- | Finalise effects and collect diagnostics
finaliseEffects ::
    Eff [Eff.Writer [ElaraWarning], Error ElaraError, IOE] a ->
    IO ([ElaraReport], Maybe a)
finaliseEffects eff = runEff $ do
    result <- runError @ElaraError $ Eff.runWriter @[ElaraWarning] eff
    case result of
        Left (callStack, err) -> do
            pure (diagnosticReports err, Nothing)
        Right (res, warnings) -> do
            let reports = concatMap diagnosticReports warnings
            pure (reports, Just res)

-- | Assert that a pipeline result succeeds
pipelineResShouldSucceed :: _ => IO ([ElaraReport], Maybe b) -> IO b
pipelineResShouldSucceed m = do
    (reports, x) <- m
    let hasErrors = any (\r -> reportSeverity r == ErrorSeverity) reports

    when hasErrors $ do
        diag <- reportsToDiagnostic reports
        expectationFailure $ toString $ prettyToText $ prettyDiagnostic' WithUnicode (TabSize 4) diag

    case x of
        Just ok -> pure ok
        Nothing -> expectationFailure "Pipeline failed without returning a result."

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
    mkFakeVarP name info = (NameOp <$> qualifiedTest name, info)

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
    intType = TypeConstructor testRegion (knownQualified (knownTypeInfo (KnownOpaque PrimInt))) []
    boolType = TypeConstructor testRegion (knownQualified (knownTypeInfo (KnownWiredIn WiredInBool))) []
    intToIntToInt = Function testRegion intType (Function testRegion intType intType)
    intToIntToBool = Function testRegion intType (Function testRegion intType boolType)

-- | Minimal rules for testing that just errors out for any query
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
    let tokenStream = TokenStream source tokens False 0
    first WParseErrorBundle <$> runParserT parser fp tokenStream

-- | Create an OpLookup from the fake operator table
fakeOpLookup :: OpLookup es
fakeOpLookup name =
    pure $ case Map.lookup name fakeOperatorTable of
        Just i -> DeclaredFixity (Just i.precedence) (Just i.associativity)
        Nothing -> DeclaredFixity Nothing Nothing

mkFakeVar :: OpName -> VarRef VarName
mkFakeVar name = Global (testLocated (qualifiedTest (OperatorVarName name)))

newtype Shown = Shown String deriving (Lift)

instance Show Shown where
    show (Shown s) = s
