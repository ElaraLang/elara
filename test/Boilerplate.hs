{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}

module Boilerplate where

import Common (diagShouldSucceed)
import Control.Exception (throwIO)
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static (Error, runError)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Local (evalState)
import Effectful.Writer.Static.Local (runWriter)
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
import Elara.Lexer.Utils (LexerError)
import Elara.Logging (StructuredDebug, ignoreStructuredDebug)
import Elara.Parse
import Elara.Parse.Error (WParseErrorBundle)
import Elara.Parse.Expression
import Elara.Prim (boolName, mkPrimQual, primModuleName)
import Elara.Prim.Rename
import Elara.Query qualified
import Elara.ReadFile
import Elara.Rename
import Elara.Rename.Error
import Elara.Shunt
import Elara.Shunt.Error (ShuntError)
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
import Rock.MemoE (Memoise, memoiseRunIO)
import Rules (testRules)
import Test.Syd (expectationFailure)
import Test.Syd.Run (mkNotEqualButShouldHaveBeenEqual)
import Text.Show

loadRenamedExpr' ::
    forall w.
    ( DiagnosticWriter (Doc AnsiStyle) :> w
    , IOE :> w
    , Error SomeReportableError :> w
    ) =>
    Text -> Eff w (Expr 'Renamed)
loadRenamedExpr' source = runQueryEffects $ do
    let fp = "<tests>"
    Elara.Error.addFile fp (toString source)
    tokens <- runErrorOrReport @LexerError $ readTokensWith (FileContents fp source)
    parsed <- runErrorOrReport @(WParseErrorBundle _ _) $ parseWith exprParser fp (source, tokens)
    desugared <- runErrorOrReport @DesugarError $ evalState mempty $ inject $ desugarExpr parsed

    runReader Nothing $
        runReader (Nothing @(Module 'Desugared)) $
            evalState operatorRenameState $
                runErrorOrReport @RenameError $
                    renameExpr desugared

runQueryEffects :: IOE :> r => Eff (Rock.Rock Elara.Query.Query : Memoise : Concurrent : FileSystem : UniqueGen : StructuredDebug : r) a -> Eff r a
runQueryEffects =
    ignoreStructuredDebug
        . uniqueGenToGlobalIO
        . runFileSystem
        . runConcurrent
        . memoiseRunIO @Elara.Query.Query
        . Rock.runRock testRules

finaliseEffects ::
    Eff
        [Error SomeReportableError, DiagnosticWriter (Doc AnsiStyle), IOE]
        a ->
    IO (Diagnostic (Doc AnsiStyle), Maybe a)
finaliseEffects eff = do
    (diagnostics, r) <- runEff $ runDiagnosticWriter $ do
        result <- runError @SomeReportableError $ eff
        case result of
            Left (callStack, error) -> do
                Elara.Error.report error
                printPretty callStack
                pure Nothing
            Right r -> pure (Just r)
    pure (diagnostics, r)

reportableErrorToMaybe :: Either SomeReportableError a -> (Diagnostic (Doc AnsiStyle), Maybe a)
reportableErrorToMaybe (Right x) = do
    (mempty, Just x)
reportableErrorToMaybe (Left error) = runPureEff $ runDiagnosticWriter $ do
    Elara.Error.report error
    pure Nothing

loadShuntedExpr :: _ => Text -> IO (Diagnostic (Doc AnsiStyle), Maybe (Expr 'Shunted))
loadShuntedExpr source = finaliseEffects $ runQueryEffects $ do
    renamed <- loadRenamedExpr' source
    runErrorOrReport @ShuntError $
        ( fst
            <$> runWriter
                (let ?lookup = lookupFromOpTable fakeOperatorTable in fixExpr renamed)
        )

pipelineResShouldSucceed :: _ => IO (Diagnostic (Doc AnsiStyle), Maybe b) -> IO b
pipelineResShouldSucceed m = do
    (d, x) <- m
    let hasErrors d =
            any (\case Err{} -> True; Warn{} -> False) $ reportsOf d
    when (hasErrors d) $
        expectationFailure $
            toString $
                prettyToText $
                    prettyDiagnostic' WithUnicode (TabSize 4) d
    case x of
        Just ok -> pure ok
        Nothing -> expectationFailure $ toString $ prettyToText $ prettyDiagnostic' WithUnicode (TabSize 4) d

evalPipelineRes :: (MonadIO m, MonadTest m) => IO (Diagnostic (Doc AnsiStyle), Maybe b) -> m b
evalPipelineRes m = do
    (d, x) <- liftIO m
    diagShouldSucceed (d, x)

    case x of
        Just ok -> pure ok
        Nothing -> failWith Nothing $ toString $ prettyToText $ prettyDiagnostic' WithUnicode (TabSize 4) d

operatorRenameState :: RenameState
operatorRenameState =
    let mkFakeVarP name = (OperatorVarName name, one $ mkFakeVar name)
     in primitiveRenameState
            <> RenameState
                { varNames =
                    fromList
                        [ mkFakeVarP "+"
                        , mkFakeVarP "-"
                        , mkFakeVarP "*"
                        , mkFakeVarP "/"
                        , mkFakeVarP "|>"
                        , mkFakeVarP "=="
                        ]
                , typeNames = fromList []
                , typeVars = fromList []
                }

fakeOperatorTable :: OpTable
fakeOperatorTable =
    let mkFakeVarP name info = (ignoreLocation $ withName $ mkFakeOp name, info)
     in fromList
            [ mkFakeVarP "+" (OpInfo (mkPrecedence 6) LeftAssociative)
            , mkFakeVarP "-" (OpInfo (mkPrecedence 6) LeftAssociative)
            , mkFakeVarP "*" (OpInfo (mkPrecedence 7) LeftAssociative)
            , mkFakeVarP "/" (OpInfo (mkPrecedence 7) LeftAssociative)
            , mkFakeVarP "|>" (OpInfo (mkPrecedence 1) RightAssociative)
            , mkFakeVarP "==" (OpInfo (mkPrecedence 4) NonAssociative)
            ]

fakeTypeEnvironment :: TypeEnvironment SourceRegion
fakeTypeEnvironment =
    let scalarBool = TypeConstructor testRegion (mkPrimQual boolName) []
     in emptyTypeEnvironment
            & addType (TermVarKey (qualifiedTest $ OperatorVarName "+")) (Lifted (Function testRegion (Scalar testRegion ScalarInt) (Function testRegion (Scalar testRegion ScalarInt) (Scalar testRegion ScalarInt))))
            & addType (TermVarKey (qualifiedTest $ OperatorVarName "-")) (Lifted (Function testRegion (Scalar testRegion ScalarInt) (Function testRegion (Scalar testRegion ScalarInt) (Scalar testRegion ScalarInt))))
            & addType (TermVarKey (qualifiedTest $ OperatorVarName "*")) (Lifted (Function testRegion (Scalar testRegion ScalarInt) (Function testRegion (Scalar testRegion ScalarInt) (Scalar testRegion ScalarInt))))
            & addType (TermVarKey (qualifiedTest $ OperatorVarName "/")) (Lifted (Function testRegion (Scalar testRegion ScalarInt) (Function testRegion (Scalar testRegion ScalarInt) (Scalar testRegion ScalarInt))))
            & addType (TermVarKey (qualifiedTest $ OperatorVarName "|>")) (Lifted (Function testRegion (Scalar testRegion ScalarInt) (Function testRegion (Scalar testRegion ScalarInt) (Scalar testRegion ScalarInt))))
            & addType (TermVarKey (qualifiedTest $ OperatorVarName "==")) (Lifted (Function testRegion (Scalar testRegion ScalarInt) (Function testRegion (Scalar testRegion ScalarInt) scalarBool)))
            & addType (DataConKey (Qualified "True" primModuleName)) (Lifted scalarBool)
            & addType (DataConKey (Qualified "False" primModuleName)) (Lifted scalarBool)

mkFakeVar :: OpName -> VarRef VarName
mkFakeVar name = Global (testLocated (qualifiedTest (OperatorVarName name)))

mkFakeOp :: OpName -> VarRef OpName
mkFakeOp name = Global (testLocated (qualifiedTest name))

ensureExpressionMatches :: HasCallStack => Q Pat -> Q Exp
ensureExpressionMatches qpat = do
    pat <- qpat
    let msg = Shown $ pprint (stripQualifiers pat)

    [e|\case $(pure pat) -> pass; o -> withFrozenCallStack $ failDiff msg o|]

-- | like 'ensureExpressionMatches', but for Expectations rather than Hedgehog properties
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
