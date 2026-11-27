-- | The Elara language server protocol (LSP) implementation.
module Elara.LSP (
    runLanguageServer,
    snapshotDiagnostics,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (modifyMVar, modifyMVar_)
import Control.Exception.Safe (tryAny)
import Language.LSP.Diagnostics (partitionBySource)
import System.Directory (makeAbsolute)
import System.FilePath (normalise)

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP

import Elara.Data.Pretty (prettyToUnannotatedText)
import Elara.Error (ElaraReport (..), ElaraSeverity (..), diagnosticReports)
import Elara.LSP.Check
import Elara.LSP.Diagnostics
import Elara.LSP.Hover (hoverAt)
import Elara.LSP.Position (zeroLSPRange)
import Elara.LSP.Utils
import Elara.Settings (CompilerSettings (..))

data ServerState = ServerState
    { baseSettings :: CompilerSettings
    {- ^ Compiler settings shared by every document.
    The 'mainFile' attribute is 'Nothing' or ignored, since 'checkDocument' checks each document as if it was the main file.
    -}
    , pending :: MVar (Set LSP.NormalizedUri)
    -- ^ documents waiting to be processed
    , wake :: MVar ()
    -- ^ filled when there is something in 'pending'
    , snapshots :: IORef (Map LSP.NormalizedUri FileSnapshot)
    -- ^ the result of the latest check of each open document
    }

-- | Run the language server over stdin/stdout until the client manually quits. Returns the exit code that 'LSP.runServer' returns.
runLanguageServer :: CompilerSettings -> IO Int
runLanguageServer settings = do
    absStdlib <- makeAbsolute settings.stdlibDir
    absSources <- traverse makeAbsolute settings.sourceDirs
    st <-
        ServerState settings{stdlibDir = absStdlib, sourceDirs = absSources}
            <$> newMVar Set.empty
            <*> newEmptyMVar
            <*> newIORef Map.empty
    LSP.runServer (serverDefinition st)

serverDefinition :: ServerState -> LSP.ServerDefinition ()
serverDefinition st =
    LSP.ServerDefinition
        { LSP.defaultConfig = ()
        , LSP.configSection = "elara"
        , LSP.parseConfig = \_ _ -> Right ()
        , LSP.onConfigChange = const pass
        , LSP.doInitialize = \env _ -> do
            _ <- forkIO (LSP.runLspT env (vacuous $ worker st))
            pure (Right env)
        , LSP.staticHandlers = const (handlers st)
        , LSP.interpretHandler = \env -> LSP.Iso (LSP.runLspT env) liftIO
        , LSP.options =
            LSP.defaultOptions
                { LSP.optTextDocumentSync =
                    Just
                        LSP.TextDocumentSyncOptions
                            { _openClose = Just True
                            , _change = Just LSP.TextDocumentSyncKind_None
                            , _willSave = Nothing
                            , _willSaveWaitUntil = Nothing
                            , _save = Just (LSP.InR (LSP.SaveOptions (Just False)))
                            }
                }
        }

handlers :: ServerState -> LSP.Handlers (LSP.LspM ())
handlers st =
    mconcat
        [ LSP.notificationHandler LSP.SMethod_Initialized (const pass) -- do nothing on initialize
        , LSP.notificationHandler LSP.SMethod_TextDocumentDidOpen $ -- process on file open
            \(LSP.TNotificationMessage _ _ (LSP.DidOpenTextDocumentParams (LSP.TextDocumentItem uri _ _ _))) ->
                enqueue st uri
        , LSP.notificationHandler LSP.SMethod_TextDocumentDidSave $ -- process on file save
            \(LSP.TNotificationMessage _ _ (LSP.DidSaveTextDocumentParams (LSP.TextDocumentIdentifier uri) _)) ->
                enqueue st uri
        , LSP.notificationHandler LSP.SMethod_TextDocumentDidClose $ -- clear cache on file close
            \(LSP.TNotificationMessage _ _ (LSP.DidCloseTextDocumentParams (LSP.TextDocumentIdentifier uri))) ->
                do
                    modifyIORef' st.snapshots (Map.delete (LSP.toNormalizedUri uri))
                    publish (LSP.toNormalizedUri uri) []
        , LSP.requestHandler LSP.SMethod_TextDocumentHover $ -- on hover, show hover info
            \(LSP.TRequestMessage _ _ _ (LSP.HoverParams (LSP.TextDocumentIdentifier uri) position _)) respond -> do
                known <- readIORef st.snapshots -- the snapshot should already be present in cache
                respond . Right . maybeToLspInl $
                    Map.lookup (LSP.toNormalizedUri uri) known >>= (`hoverAt` position)
        , LSP.notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ -- nothing to do here, plugin's job to restart server
            const pass
        ]

-- | Queue a document to be checked
enqueue :: ServerState -> LSP.Uri -> LSP.LspM () ()
enqueue st uri = do
    liftIO $ modifyMVar_ st.pending (pure . Set.insert (LSP.toNormalizedUri uri))
    void (tryPutMVar st.wake ())

-- | Checks and published queued documents in 'pending' every 150ms
worker :: ServerState -> LSP.LspM () Void
worker st = infinitely $ do
    takeMVar st.wake *> liftIO (threadDelay 150_000)
    uris <- liftIO $ modifyMVar st.pending (\queued -> pure (Set.empty, queued))
    for_ uris $ \uri -> do
        checkAndPublish st uri

insertSnapshot :: MonadIO m => ServerState -> LSP.NormalizedUri -> FileSnapshot -> m ()
insertSnapshot st uri snapshot = liftIO (modifyIORef' st.snapshots (Map.insert uri snapshot))

-- | Check a file and publish its result to the client
checkAndPublish :: ServerState -> LSP.NormalizedUri -> LSP.LspM () ()
checkAndPublish st uri = whenJust (LSP.uriToFilePath (LSP.fromNormalizedUri uri)) $ \file -> do
    path <- liftIO (normalise <$> makeAbsolute file)
    snapshot <- liftIO (checkDocument st.baseSettings path)
    insertSnapshot st uri snapshot

    -- errors can point into other files (imported modules), and we need their text to turn those into ranges
    let blockerReports =
            -- all reports from the error
            maybe [] diagnosticReports snapshot.snapshotBlocker
        otherFiles =
            -- any _other_ file paths mentioned in the error
            filter (/= path) (ordNub (concatMap reportFiles (snapshot.snapshotReports <> blockerReports)))

    -- the textual contents of the 'otherFiles', where possible to read
    otherTexts <- fmap (Map.fromList . catMaybes) $ for otherFiles $ \otherFilePath ->
        fmap (\bytes -> (otherFilePath, decodeUtf8 bytes)) . rightToMaybe <$> tryAny (readFileBS otherFilePath)

    let readFile' other =
            if other == path
                then Just snapshot.snapshotText
                else Map.lookup other otherTexts

    publish uri (snapshotDiagnostics readFile' snapshot)

-- | Filter the diagnostics that belong in the document that was checked
snapshotDiagnostics ::
    -- | read/lookup a file's content if known
    (FilePath -> Maybe Text) ->
    FileSnapshot ->
    [LSP.Diagnostic]
snapshotDiagnostics readFile' snapshot = mainDiagnostics <> transitiveDiagnostic
  where
    blockerReports = maybe [] diagnosticReports snapshot.snapshotBlocker

    mainDiagnostics = reportToDiagnostic readFile' <$> ownReports snapshot

    -- if we couldn't type check this file because of something in another file, say so,
    -- otherwise the file would look fine while nothing about it works
    transitiveDiagnostic
        | couldNotCheck = [blockedDiagnostic]
        | otherwise = []

    couldNotCheck =
        isNothing (snapshotTyped snapshot)
            && isJust (snapshotBlocker snapshot)
            && any ((== ErrorSeverity) . reportSeverity) (snapshotReports snapshot)
            && null (ownReports snapshot)

    blockedDiagnostic =
        LSP.Diagnostic
            { _range = zeroLSPRange
            , _severity = Just LSP.DiagnosticSeverity_Error
            , _code = Nothing
            , _codeDescription = Nothing
            , _source = Just diagnosticSource
            , _message = "This module could not be checked because of errors in other modules"
            , _tags = Nothing
            , _relatedInformation = Just blockerLocations
            , _data_ = Nothing
            }

    -- point at (some of) the errors that are the real problem
    blockerLocations = mapMaybe blockerLocation (take 10 blockerReports)

    blockerLocation report = do
        location <- reportLocation readFile' report
        pure (LSP.DiagnosticRelatedInformation location (prettyToUnannotatedText (reportMessage report)))

-- | Replace all the diagnostics shown to the client for a document.
publish :: LSP.NormalizedUri -> [LSP.Diagnostic] -> LSP.LspM () ()
publish uri diagnostics
    | null diagnostics = LSP.flushDiagnosticsBySourceAndUri maxDiagnostics (Just diagnosticSource) uri
    | otherwise = LSP.publishDiagnostics maxDiagnostics uri Nothing (partitionBySource diagnostics)
  where
    maxDiagnostics = 100
