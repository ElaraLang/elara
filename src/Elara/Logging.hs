{-# LANGUAGE AllowAmbiguousTypes #-}

module Elara.Logging (
    -- * Log Levels
    LogLevel (..),

    -- * Configuration
    LogConfig (..),
    defaultLogConfig,
    getLogConfigFromEnv,

    -- * Structured Debug Effect
    StructuredDebug,

    -- * Basic Logging Functions
    logDebug,
    logInfo,
    logWarning,
    logError,

    -- * Namespaced Logging Functions
    logDebugNS,
    logInfoNS,
    logWarningNS,
    logErrorNS,

    -- * Scoped Logging Functions
    logDebugWith,
    logInfoWith,
    logDebugWithNS,
    logInfoWithNS,

    -- * Namespace Context
    withNamespace,

    -- * Legacy Functions (for backward compatibility)
    debug,
    debugWith,
    debugWithResult,
    debugNS,
    debugWithNS,

    -- * Interpreters
    structuredDebugToLog,
    structuredDebugToLogWith,
    ignoreStructuredDebug,

    -- * Traceable Functions
    TraceableFn (..),
    runTraceable,
    traceFn,
    fib,

    -- * Internal (exported for testing)
    shouldLog,
) where

import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, liftIO, (:>))
import Effectful.Colog qualified as Log
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift, reinterpret, send)
import Effectful.State.Static.Local qualified as S
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style
import GHC.Exts
import GHC.Stack (callStack, getCallStack, srcLocFile, srcLocStartLine)
import GHC.TypeLits (KnownSymbol (..), symbolVal)
import System.Environment (lookupEnv)

-- | Log levels for structured debug messages
data LogLevel
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show)

instance Pretty LogLevel where
    pretty Debug = Style.keyword "DEBUG"
    pretty Info = Style.varName "INFO"
    pretty Warning = Style.warning "WARN"
    pretty Error = Style.errorCode "ERROR"

-- | Configuration for the structured debug system
data LogConfig = LogConfig
    { minLogLevel :: LogLevel
    -- ^ Minimum log level to display
    , showTimestamps :: Bool
    -- ^ Whether to show timestamps in log messages
    , showSourceLoc :: Bool
    -- ^ Whether to show source location (file:line) in log messages
    , namespaceFilter :: Maybe [T.Text]
    -- ^ Optional namespace filter - only log messages matching this prefix
    }
    deriving (Eq, Show)

-- | Default log configuration
-- Optimized for readability during simple debugging - timestamps and source locations are disabled by default
-- Enable them via environment variables when needed for detailed diagnostics
defaultLogConfig :: LogConfig
defaultLogConfig =
    LogConfig
        { minLogLevel = Debug
        , showTimestamps = False
        , showSourceLoc = False
        , namespaceFilter = Nothing
        }

-- | Parse a boolean value from environment variable
-- Accepts: "true", "True", "TRUE", "1", "yes", "Yes", "YES"
-- Anything else (including Nothing) is treated as False
parseBool :: Maybe String -> Bool
parseBool Nothing = False
parseBool (Just s) = map toLower s `elem` ["true", "1", "yes"]
  where
    toLower c
        | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
        | otherwise = c

-- | Get log configuration from environment variables
-- Environment variables:
--   - ELARA_DEBUG: If set, enables debug level logging
--   - ELARA_LOG_LEVEL: Set to DEBUG, INFO, WARN, or ERROR
--   - ELARA_LOG_TIMESTAMPS: Set to "true"/"1"/"yes" (case-insensitive) to enable timestamps (default: false)
--   - ELARA_LOG_SOURCE_LOC: Set to "true"/"1"/"yes" (case-insensitive) to enable source locations (default: false)
--   - ELARA_LOG_NAMESPACE: Filter logs to only show messages from this namespace (dot-separated)
getLogConfigFromEnv :: IO LogConfig
getLogConfigFromEnv = do
    debugEnabled <- lookupEnv "ELARA_DEBUG"
    logLevel <- lookupEnv "ELARA_LOG_LEVEL"
    showTimestamps <- lookupEnv "ELARA_LOG_TIMESTAMPS"
    showSourceLoc <- lookupEnv "ELARA_LOG_SOURCE_LOC"
    namespaceFilter <- lookupEnv "ELARA_LOG_NAMESPACE"

    let minLevel = case logLevel of
            Just "ERROR" -> Error
            Just "WARN" -> Warning
            Just "INFO" -> Info
            Just "DEBUG" -> Debug
            _ -> if isJust debugEnabled then Debug else Info

    pure $
        LogConfig
            { minLogLevel = minLevel
            , showTimestamps = parseBool showTimestamps
            , showSourceLoc = parseBool showSourceLoc
            , namespaceFilter = fmap (T.split (== '.') . T.pack) namespaceFilter
            }

data StructuredDebug :: Effect where
    -- Legacy debug operations (maintained for backwards compatibility)
    DebugOld :: HasCallStack => Doc AnsiStyle -> StructuredDebug m ()
    DebugWith :: HasCallStack => Doc AnsiStyle -> m a -> StructuredDebug m a
    DebugNS :: HasCallStack => [T.Text] -> Doc AnsiStyle -> StructuredDebug m ()
    DebugWithNS :: HasCallStack => [T.Text] -> Doc AnsiStyle -> m a -> StructuredDebug m a
    -- New operations with log levels
    LogMsg :: HasCallStack => LogLevel -> Doc AnsiStyle -> StructuredDebug m ()
    LogMsgNS :: HasCallStack => LogLevel -> [T.Text] -> Doc AnsiStyle -> StructuredDebug m ()
    LogWith :: HasCallStack => LogLevel -> Doc AnsiStyle -> m a -> StructuredDebug m a
    LogWithNS :: HasCallStack => LogLevel -> [T.Text] -> Doc AnsiStyle -> m a -> StructuredDebug m a
    -- Namespace context operation
    WithNamespace :: HasCallStack => [T.Text] -> m a -> StructuredDebug m a

type instance DispatchOf StructuredDebug = 'Dynamic

-- | Legacy debug function (maps to Debug level)
debug :: HasCallStack => StructuredDebug :> r => Doc AnsiStyle -> Eff r ()
debug msg = send $ DebugOld msg

debugWith :: HasCallStack => StructuredDebug :> r => Doc AnsiStyle -> Eff r a -> Eff r a
debugWith msg act = send $ DebugWith msg act

debugWithResult :: (StructuredDebug :> r, Pretty a) => Doc AnsiStyle -> Eff r a -> Eff r a
debugWithResult msg act = debugWith msg $ do
    res <- act
    debug ("Result: " <> pretty res)
    pure res

-- namespaced helpers
debugNS :: (StructuredDebug :> r, HasCallStack) => [T.Text] -> Doc AnsiStyle -> Eff r ()
debugNS ns msg = send $ DebugNS ns msg

debugWithNS :: (StructuredDebug :> r, HasCallStack) => [T.Text] -> Doc AnsiStyle -> Eff r a -> Eff r a
debugWithNS ns msg act = send $ DebugWithNS ns msg act

-- | New logging functions with explicit log levels

-- | Log a message at Debug level
logDebug :: HasCallStack => StructuredDebug :> r => Doc AnsiStyle -> Eff r ()
logDebug = send . LogMsg Debug

-- | Log a message at Info level
logInfo :: HasCallStack => StructuredDebug :> r => Doc AnsiStyle -> Eff r ()
logInfo = send . LogMsg Info

-- | Log a message at Warning level
logWarning :: HasCallStack => StructuredDebug :> r => Doc AnsiStyle -> Eff r ()
logWarning = send . LogMsg Warning

-- | Log a message at Error level
logError :: HasCallStack => StructuredDebug :> r => Doc AnsiStyle -> Eff r ()
logError = send . LogMsg Error

-- | Log a message at Debug level with namespace
logDebugNS :: HasCallStack => StructuredDebug :> r => [T.Text] -> Doc AnsiStyle -> Eff r ()
logDebugNS ns = send . LogMsgNS Debug ns

-- | Log a message at Info level with namespace
logInfoNS :: HasCallStack => StructuredDebug :> r => [T.Text] -> Doc AnsiStyle -> Eff r ()
logInfoNS ns = send . LogMsgNS Info ns

-- | Log a message at Warning level with namespace
logWarningNS :: HasCallStack => StructuredDebug :> r => [T.Text] -> Doc AnsiStyle -> Eff r ()
logWarningNS ns = send . LogMsgNS Warning ns

-- | Log a message at Error level with namespace
logErrorNS :: HasCallStack => StructuredDebug :> r => [T.Text] -> Doc AnsiStyle -> Eff r ()
logErrorNS ns = send . LogMsgNS Error ns

-- | Execute an action with a debug log message showing entry/exit
logDebugWith :: HasCallStack => StructuredDebug :> r => Doc AnsiStyle -> Eff r a -> Eff r a
logDebugWith msg = send . LogWith Debug msg

-- | Execute an action with an info log message showing entry/exit
logInfoWith :: HasCallStack => StructuredDebug :> r => Doc AnsiStyle -> Eff r a -> Eff r a
logInfoWith msg = send . LogWith Info msg

-- | Execute an action with a debug log message and namespace
logDebugWithNS :: HasCallStack => StructuredDebug :> r => [T.Text] -> Doc AnsiStyle -> Eff r a -> Eff r a
logDebugWithNS ns msg = send . LogWithNS Debug ns msg

-- | Execute an action with an info log message and namespace
logInfoWithNS :: HasCallStack => StructuredDebug :> r => [T.Text] -> Doc AnsiStyle -> Eff r a -> Eff r a
logInfoWithNS ns msg = send . LogWithNS Info ns msg

-- | Set a namespace context for a block of code
-- All log messages within this block will automatically include the namespace
-- without needing to specify it explicitly.
--
-- Example:
-- @
-- withNamespace ["TypeInfer", "Unification"] $ do
--     logDebug "Unifying types"  -- Automatically includes namespace
--     logInfo "Unification complete"
-- @
withNamespace :: HasCallStack => StructuredDebug :> r => [T.Text] -> Eff r a -> Eff r a
withNamespace ns act = send $ WithNamespace ns act

-- | Extract source location from HasCallStack
getSourceLoc :: HasCallStack => Maybe (String, Int)
getSourceLoc = case getCallStack callStack of
    [] -> Nothing
    ((_, loc) : _) -> Just (srcLocFile loc, srcLocStartLine loc)

-- | Format a log message with optional timestamp and source location
formatLogMessage ::
    LogConfig ->
    LogLevel ->
    [T.Text] ->
    Int ->
    Doc AnsiStyle ->
    Maybe (String, Int) ->
    IO (Doc AnsiStyle)
formatLogMessage config level ns depth msg srcLoc = do
    timestamp <-
        if showTimestamps config
            then do
                now <- getCurrentTime
                let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
                pure $ Style.punctuation (pretty timeStr) <> " "
            else pure mempty

    let levelDoc = "[" <> pretty level <> "]" <> " "
        nsDoc = if null ns then mempty else pretty ("[" <> T.intercalate "." ns <> "] ")
        locDoc = case srcLoc of
            Just (file, line) | showSourceLoc config ->
                Style.punctuation (pretty file <> ":" <> pretty line) <> " "
            _ -> mempty
        prefix = stimes depth "│ "
        indentedMsg = prefix <> hang (2 * depth) msg

    pure $ timestamp <> levelDoc <> locDoc <> nsDoc <> indentedMsg

-- | Check if a message should be logged based on configuration
shouldLog :: LogConfig -> LogLevel -> [T.Text] -> Bool
shouldLog config level ns =
    level >= minLogLevel config
        && case namespaceFilter config of
            Nothing -> True
            Just filterNs -> filterNs `isPrefixOf` ns

structuredDebugToLog :: forall r a. (HasCallStack, IOE :> r) => Log.Log (Doc AnsiStyle) :> r => Eff (StructuredDebug : r) a -> Eff r a
structuredDebugToLog = structuredDebugToLogWith defaultLogConfig

-- | Interpret StructuredDebug with custom configuration
structuredDebugToLogWith :: forall r a. (HasCallStack, IOE :> r) => LogConfig -> Log.Log (Doc AnsiStyle) :> r => Eff (StructuredDebug : r) a -> Eff r a
structuredDebugToLogWith config = reinterpret (S.evalState ([] :: [T.Text]) . S.evalState (0 :: Int)) $ \env -> \case
    -- Legacy operations (treated as Debug level)
    DebugOld msg -> logHelper Debug [] msg getSourceLoc
    DebugNS names msg -> logHelper Debug names msg getSourceLoc
    DebugWith msg act -> logWithHelper Debug [] msg act
    DebugWithNS names msg act -> logWithHelper Debug names msg act
    -- New operations with explicit log levels
    LogMsg level msg -> logHelper level [] msg getSourceLoc
    LogMsgNS level names msg -> logHelper level names msg getSourceLoc
    LogWith level msg act -> logWithHelper level [] msg act
    LogWithNS level names msg act -> logWithHelper level names msg act
    -- Namespace context operation
    WithNamespace names act -> withNamespaceHelper names act
  where
    logHelper :: LogLevel -> [T.Text] -> Doc AnsiStyle -> Maybe (String, Int) -> Eff (StructuredDebug : r) ()
    logHelper level names msg srcLoc = do
        depth <- S.get @Int
        ns <- S.get @[T.Text]
        let fullNs = ns <> names
        when (shouldLog config level fullNs) $ do
            formatted <- liftIO $ formatLogMessage config level fullNs depth msg srcLoc
            Log.logMsg formatted

    logWithHelper :: LogLevel -> [T.Text] -> Doc AnsiStyle -> Eff (StructuredDebug : r) a -> Eff (StructuredDebug : r) a
    logWithHelper level names msg act = do
        depth <- S.get @Int
        ns <- S.get @[T.Text]
        let fullNs = ns <> names
        when (shouldLog config level fullNs) $ do
            formatted <- liftIO $ formatLogMessage config level fullNs depth msg getSourceLoc
            Log.logMsg formatted
        S.put (depth + 1)
        S.put fullNs
        res <- localSeqUnlift env $ \unlift -> unlift act
        S.put depth
        S.put ns
        pure res

    withNamespaceHelper :: [T.Text] -> Eff (StructuredDebug : r) a -> Eff (StructuredDebug : r) a
    withNamespaceHelper names act = do
        ns <- S.get @[T.Text]
        let fullNs = ns <> names
        S.put fullNs
        res <- localSeqUnlift env $ \unlift -> unlift act
        S.put ns
        pure res

ignoreStructuredDebug :: Eff (StructuredDebug : r) a -> Eff r a
ignoreStructuredDebug = interpret $ \env -> \case
    DebugOld _ -> pass
    DebugWith _ act -> localSeqUnlift env $ \unlift -> unlift act
    DebugNS _ _ -> pass
    DebugWithNS _ _ act -> localSeqUnlift env $ \unlift -> unlift act
    LogMsg _ _ -> pass
    LogMsgNS _ _ _ -> pass
    LogWith _ _ act -> localSeqUnlift env $ \unlift -> unlift act
    LogWithNS _ _ _ act -> localSeqUnlift env $ \unlift -> unlift act
    WithNamespace _ act -> localSeqUnlift env $ \unlift -> unlift act

{- | Inspired by https://x.com/Quelklef/status/1860188828876583146 !
A recursive, pure function, which can be traced with a monadic effect.
-}
newtype TraceableFn (name :: Symbol) a b
    = TraceableFn (forall m. Monad m => (a -> m b) -> a -> m b)

-- | Purely run a traceable function, without any tracing
runTraceable :: TraceableFn name a b -> a -> b
runTraceable (TraceableFn f) a = runIdentity $ do
    f (pure . runTraceable (TraceableFn f)) a

-- | Run a traceable function with structured debug tracing
traceFn ::
    forall (name :: Symbol) a b r.
    (Pretty a, Pretty b, StructuredDebug :> r, KnownSymbol name) =>
    TraceableFn name a b -> (a -> Eff r b)
traceFn (TraceableFn f) a = do
    let p = symbolVal (Proxy @name)
    res <- debugWith (pretty p <> ":" <+> pretty a) $ do
        f (traceFn @name (TraceableFn f)) a
    debug $ pretty res
    pure res

fib :: TraceableFn "fib" Int Int
fib = TraceableFn $ \f n ->
    case n of
        0 -> pure 0
        1 -> pure 1
        _ -> do
            a <- f (n - 1)
            b <- f (n - 2)
            pure $ a + b
