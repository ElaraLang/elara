{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Elara.Logging (
    -- * Co-log RichMessage Integration
    ElaraMessage (..),
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

    -- * Co-log utilities
    formatRichMessage,
    filterByLevel,
    filterByNamespace,
) where

import Colog.Core.Action (LogAction (..), cmap, cmapM)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Colog qualified as Log
import Effectful.Dispatch.Dynamic (LocalEnv, interpret, localSeqUnlift, reinterpret, send)
import Effectful.State.Static.Local qualified as S
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style
import GHC.Exts
import GHC.Stack (srcLocFile, srcLocStartLine)
import GHC.TypeLits (KnownSymbol (..), symbolVal)
import Relude.Extra (safeToEnum)

{- | Log levels for structured debug messages
Using our own type instead of co-log's Severity for better integration with Pretty
-}
data LogLevel
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show, Generic)

instance Pretty LogLevel where
    pretty Debug = Style.keyword "DEBUG"
    pretty Info = Style.varName "INFO"
    pretty Warning = Style.warning "WARN"
    pretty Error = Style.error "ERROR"

{- | Rich message type for Elara logging - extends co-log's RichMessage pattern
This contains all the context we need for structured logging
-}
data ElaraMessage = ElaraMessage
    { emLevel :: !LogLevel
    -- ^ Log severity level
    , emMessage :: !(Doc AnsiStyle)
    -- ^ The actual log message
    , emNamespace :: ![T.Text]
    -- ^ Namespace hierarchy
    , emDepth :: !Int
    -- ^ Indentation depth for hierarchical logging
    , emStack :: !(Maybe CallStack)
    -- ^ Call stack for source location
    , emTime :: !(Maybe UTCTime)
    -- ^ Timestamp (populated by enrichment)
    }
    deriving (Generic)

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

{- | Default log configuration
Optimized for readability during simple debugging - timestamps and source locations are disabled by default
Enable them via environment variables when needed for detailed diagnostics
-}
defaultLogConfig :: LogConfig
defaultLogConfig =
    LogConfig
        { minLogLevel = Debug
        , showTimestamps = False
        , showSourceLoc = False
        , namespaceFilter = Nothing
        }

{- | Parse a boolean value from environment variable
Accepts: "true", "True", "TRUE", "1", "yes", "Yes", "YES"
Anything else (including Nothing) is treated as False
-}
parseBool :: Maybe Text -> Bool
parseBool Nothing = False
parseBool (Just s) = T.toLower s `elem` ["true", "1", "yes"]

{- | Get log configuration from environment variables
Environment variables:
  - ELARA_DEBUG: If set, enables debug level logging
  - ELARA_LOG_LEVEL: Set to DEBUG, INFO, WARN, or ERROR
  - ELARA_LOG_TIMESTAMPS: Set to "true"/"1"/"yes" (case-insensitive) to enable timestamps (default: false)
  - ELARA_LOG_SOURCE_LOC: Set to "true"/"1"/"yes" (case-insensitive) to enable source locations (default: false)
  - ELARA_LOG_NAMESPACE: Filter logs to only show messages from this namespace (dot-separated)
-}
getLogConfigFromEnv :: IO LogConfig
getLogConfigFromEnv = do
    debugEnabled <- lookupEnv "ELARA_DEBUG"
    logLevel <- lookupEnv "ELARA_LOG_LEVEL"
    showTimestamps <- fmap toText <$> lookupEnv "ELARA_LOG_TIMESTAMPS"
    showSourceLoc <- fmap toText <$> lookupEnv "ELARA_LOG_SOURCE_LOC"
    namespaceFilter <- fmap toText <$> lookupEnv "ELARA_LOG_NAMESPACE"

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
            , namespaceFilter = fmap (T.split (== '.') . toText) namespaceFilter
            }

{- | Co-log filter by log level
Uses co-log's pattern of filtering messages based on severity
-}
filterByLevel :: Applicative m => LogConfig -> LogAction m ElaraMessage -> LogAction m ElaraMessage
filterByLevel config (LogAction action) = LogAction $ \msg ->
    when (emLevel msg >= minLogLevel config) $ action msg

{- | Co-log filter by namespace
Uses co-log's pattern of filtering messages based on custom fields
-}
filterByNamespace :: Applicative m => LogConfig -> LogAction m ElaraMessage -> LogAction m ElaraMessage
filterByNamespace config (LogAction action) = LogAction $ \msg ->
    let shouldLog = case namespaceFilter config of
            Nothing -> True
            Just filterNs -> filterNs `isPrefixOf` emNamespace msg
     in when shouldLog $ action msg

-- | Enrich ElaraMessage with timestamp (co-log enrichment pattern)
enrichWithTime :: IOE :> r => LogAction (Eff r) ElaraMessage -> LogAction (Eff r) ElaraMessage
enrichWithTime = cmapM $ \msg -> do
    time <- liftIO getCurrentTime
    pure msg{emTime = Just time}

-- | Enrich ElaraMessage with call stack (co-log enrichment pattern)
enrichWithStack :: HasCallStack => LogAction m ElaraMessage -> LogAction m ElaraMessage
enrichWithStack = cmap $ \msg ->
    msg{emStack = Just callStack}

{- | Format ElaraMessage to Doc AnsiStyle using co-log's cmap pattern
This is where we convert our structured message to the final output format
-}
formatRichMessage :: LogConfig -> ElaraMessage -> Doc AnsiStyle
formatRichMessage config msg =
    let timestampDoc =
            case emTime msg of
                Just time
                    | showTimestamps config ->
                        let timeStr = show time -- Simple formatting for now
                         in Style.punctuation (pretty (take 19 timeStr)) <> " "
                _ -> mempty

        levelDoc = "[" <> pretty (emLevel msg) <> "]" <> " "

        nsDoc =
            if null (emNamespace msg)
                then mempty
                else pretty ("[" <> T.intercalate "." (emNamespace msg) <> "] ")

        locDoc =
            case emStack msg of
                Just stack | showSourceLoc config ->
                    case getCallStack stack of
                        ((_, loc) : _) ->
                            Style.punctuation (pretty (srcLocFile loc) <> ":" <> pretty (srcLocStartLine loc)) <> " "
                        [] -> mempty
                _ -> mempty

        prefix = stimes (emDepth msg) "│ "
        indentedMsg = prefix <> hang (2 * emDepth msg) (emMessage msg)
     in timestampDoc <> levelDoc <> locDoc <> nsDoc <> indentedMsg

data StructuredDebug :: Effect where
    -- Legacy debug operations (maintained for backwards compatibility)
    DebugOld :: CallStack -> Doc AnsiStyle -> StructuredDebug m ()
    DebugWith :: CallStack -> Doc AnsiStyle -> m a -> StructuredDebug m a
    DebugNS :: CallStack -> [T.Text] -> Doc AnsiStyle -> StructuredDebug m ()
    DebugWithNS :: CallStack -> [T.Text] -> Doc AnsiStyle -> m a -> StructuredDebug m a
    -- New operations with log levels
    LogMsg :: CallStack -> LogLevel -> Doc AnsiStyle -> StructuredDebug m ()
    LogMsgNS :: CallStack -> LogLevel -> [T.Text] -> Doc AnsiStyle -> StructuredDebug m ()
    LogWith :: CallStack -> LogLevel -> Doc AnsiStyle -> m a -> StructuredDebug m a
    LogWithNS :: CallStack -> LogLevel -> [T.Text] -> Doc AnsiStyle -> m a -> StructuredDebug m a
    -- Namespace context operation
    WithNamespace :: CallStack -> [T.Text] -> m a -> StructuredDebug m a

type instance DispatchOf StructuredDebug = 'Dynamic

-- | Legacy debug function (maps to Debug level)
debug :: HasCallStack => StructuredDebug :> r => Doc AnsiStyle -> Eff r ()
debug msg = withFrozenCallStack $ send $ DebugOld callStack msg

debugWith :: HasCallStack => StructuredDebug :> r => Doc AnsiStyle -> Eff r a -> Eff r a
debugWith msg act = withFrozenCallStack $ send $ DebugWith callStack msg act

debugWithResult :: (StructuredDebug :> r, Pretty a) => Doc AnsiStyle -> Eff r a -> Eff r a
debugWithResult msg act = withFrozenCallStack $ debugWith msg $ do
    res <- act
    debug ("Result: " <> pretty res)
    pure res

-- namespaced helpers
debugNS :: (StructuredDebug :> r, HasCallStack) => [T.Text] -> Doc AnsiStyle -> Eff r ()
debugNS ns msg = withFrozenCallStack $ send $ DebugNS callStack ns msg

debugWithNS :: (StructuredDebug :> r, HasCallStack) => [T.Text] -> Doc AnsiStyle -> Eff r a -> Eff r a
debugWithNS ns msg act = withFrozenCallStack $ send $ DebugWithNS callStack ns msg act

-- | New logging functions with explicit log levels

-- | Log a message at Debug level
logDebug :: HasCallStack => StructuredDebug :> r => Doc AnsiStyle -> Eff r ()
logDebug = withFrozenCallStack $ send . LogMsg callStack Debug

-- | Log a message at Info level
logInfo :: HasCallStack => StructuredDebug :> r => Doc AnsiStyle -> Eff r ()
logInfo = withFrozenCallStack $ send . LogMsg callStack Info

-- | Log a message at Warning level
logWarning :: HasCallStack => StructuredDebug :> r => Doc AnsiStyle -> Eff r ()
logWarning = withFrozenCallStack $ send . LogMsg callStack Warning

-- | Log a message at Error level
logError :: HasCallStack => StructuredDebug :> r => Doc AnsiStyle -> Eff r ()
logError = withFrozenCallStack $ send . LogMsg callStack Error

-- | Log a message at Debug level with namespace
logDebugNS :: HasCallStack => StructuredDebug :> r => [T.Text] -> Doc AnsiStyle -> Eff r ()
logDebugNS ns = withFrozenCallStack $ send . LogMsgNS callStack Debug ns

-- | Log a message at Info level with namespace
logInfoNS :: HasCallStack => StructuredDebug :> r => [T.Text] -> Doc AnsiStyle -> Eff r ()
logInfoNS ns = withFrozenCallStack $ send . LogMsgNS callStack Info ns

-- | Log a message at Warning level with namespace
logWarningNS :: HasCallStack => StructuredDebug :> r => [T.Text] -> Doc AnsiStyle -> Eff r ()
logWarningNS ns = withFrozenCallStack $ send . LogMsgNS callStack Warning ns

-- | Log a message at Error level with namespace
logErrorNS :: HasCallStack => StructuredDebug :> r => [T.Text] -> Doc AnsiStyle -> Eff r ()
logErrorNS ns = withFrozenCallStack $ send . LogMsgNS callStack Error ns

-- | Execute an action with a debug log message showing entry/exit
logDebugWith :: HasCallStack => StructuredDebug :> r => Doc AnsiStyle -> Eff r a -> Eff r a
logDebugWith msg = withFrozenCallStack $ send . LogWith callStack Debug msg

-- | Execute an action with an info log message showing entry/exit
logInfoWith :: HasCallStack => StructuredDebug :> r => Doc AnsiStyle -> Eff r a -> Eff r a
logInfoWith msg = withFrozenCallStack $ send . LogWith callStack Info msg

-- | Execute an action with a debug log message and namespace
logDebugWithNS :: HasCallStack => StructuredDebug :> r => [T.Text] -> Doc AnsiStyle -> Eff r a -> Eff r a
logDebugWithNS ns msg = withFrozenCallStack $ send . LogWithNS callStack Debug ns msg

-- | Execute an action with an info log message and namespace
logInfoWithNS :: HasCallStack => StructuredDebug :> r => [T.Text] -> Doc AnsiStyle -> Eff r a -> Eff r a
logInfoWithNS ns msg = withFrozenCallStack $ send . LogWithNS callStack Info ns msg

{- | Set a namespace context for a block of code
All log messages within this block will automatically include the namespace
without needing to specify it explicitly.

Example:
@
withNamespace ["TypeInfer", "Unification"] $ do
    logDebug "Unifying types"  -- Automatically includes namespace
    logInfo "Unification complete"
@
-}
withNamespace :: HasCallStack => StructuredDebug :> r => [T.Text] -> Eff r a -> Eff r a
withNamespace ns act = withFrozenCallStack $ send $ WithNamespace callStack ns act

-- | Build a co-log LogAction that applies configuration-based filtering and formatting
buildLogAction :: forall r. (IOE :> r, Log.Log (Doc AnsiStyle) :> r) => LogConfig -> LogAction (Eff r) ElaraMessage
buildLogAction config =
    let
        -- Start with the base action that logs the formatted message
        baseAction = LogAction $ \msg ->
            Log.logMsg (formatRichMessage config msg)

        -- Apply enrichment if timestamps are enabled
        enriched =
            if config.showTimestamps
                then enrichWithTime baseAction
                else baseAction

        -- Apply filters using co-log's composable pattern
        filtered =
            filterByLevel config $
                filterByNamespace config enriched
     in
        filtered

structuredDebugToLog :: forall r a. (HasCallStack, IOE :> r) => Log.Log (Doc AnsiStyle) :> r => Eff (StructuredDebug : r) a -> Eff r a
structuredDebugToLog = structuredDebugToLogWith defaultLogConfig

{- | Interpret StructuredDebug using co-log's RichMessage pattern
This leverages co-log's LogAction combinators for filtering and enrichment
-}
structuredDebugToLogWith :: forall r a. (HasCallStack, IOE :> r) => LogConfig -> Log.Log (Doc AnsiStyle) :> r => Eff (StructuredDebug : r) a -> Eff r a
structuredDebugToLogWith config = reinterpret @_ (S.evalState ([] :: [T.Text]) . S.evalState (0 :: Int)) $ \env -> \case
    -- Legacy operations (treated as Debug level)
    DebugOld stack msg -> logMessage stack Debug [] msg
    DebugNS stack names msg -> logMessage stack Debug names msg
    DebugWith stack msg act -> logWithScope env stack Debug [] msg act
    DebugWithNS stack names msg act -> logWithScope env stack Debug names msg act
    -- New operations with explicit log levels
    LogMsg stack level msg -> logMessage stack level [] msg
    LogMsgNS stack level names msg -> logMessage stack level names msg
    LogWith stack level msg act -> logWithScope env stack level [] msg act
    LogWithNS stack level names msg act -> logWithScope env stack level names msg act
    -- Namespace context operation
    WithNamespace stack names act -> withNamespaceHelper env names act
  where
    -- Get the co-log LogAction with all filters and enrichments applied
    logAction :: LogAction (Eff (S.State Int : S.State [T.Text] : r)) ElaraMessage
    logAction = buildLogAction config

    -- Helper to create and log an ElaraMessage
    logMessage :: CallStack -> LogLevel -> [T.Text] -> Doc AnsiStyle -> Eff (S.State Int : S.State [T.Text] : r) ()
    logMessage stack level names msg = do
        depth <- S.get @Int
        ns <- S.get @[T.Text]
        let fullNs = ns <> names
            elaraMsg =
                ElaraMessage
                    { emLevel = level
                    , emMessage = msg
                    , emNamespace = fullNs
                    , emDepth = depth
                    , emStack = if config.showSourceLoc then Just stack else Nothing
                    , emTime = Nothing -- Will be enriched by LogAction if needed
                    }
        -- Use co-log's LogAction to handle filtering, enrichment, and formatting
        unLogAction logAction elaraMsg

    -- Helper for scoped logging with indentation
    logWithScope :: LocalEnv localEs (S.State Int : S.State [T.Text] : r) -> CallStack -> LogLevel -> [T.Text] -> Doc AnsiStyle -> Eff localEs b -> Eff (S.State Int : S.State [T.Text] : r) b
    logWithScope env stack level names msg act = do
        depth <- S.get @Int
        ns <- S.get @[T.Text]
        let fullNs = ns <> names
            elaraMsg =
                ElaraMessage
                    { emLevel = level
                    , emMessage = msg
                    , emNamespace = fullNs
                    , emDepth = depth
                    , emStack = if showSourceLoc config then Just stack else Nothing
                    , emTime = Nothing
                    }
        -- Log the entry message
        unLogAction logAction elaraMsg
        -- Increase depth and update namespace for nested logs
        S.put (depth + 1)
        S.put fullNs
        res <- localSeqUnlift env $ \unlift -> unlift act
        -- Restore depth and namespace
        S.put depth
        S.put ns
        pure res

    -- Helper for namespace context
    withNamespaceHelper :: LocalEnv localEs (S.State Int : S.State [T.Text] : r) -> [T.Text] -> Eff localEs b -> Eff (S.State Int : S.State [T.Text] : r) b
    withNamespaceHelper env names act = do
        ns <- S.get @[T.Text]
        let fullNs = ns <> names
        S.put fullNs
        res <- localSeqUnlift env $ \unlift -> unlift act
        S.put ns
        pure res

    -- Extract the action from LogAction
    unLogAction :: LogAction (Eff (S.State Int : S.State [T.Text] : r)) ElaraMessage -> ElaraMessage -> Eff (S.State Int : S.State [T.Text] : r) ()
    unLogAction (LogAction action) = action

ignoreStructuredDebug :: Eff (StructuredDebug : r) a -> Eff r a
ignoreStructuredDebug = interpret $ \env -> \case
    DebugOld{} -> pass
    DebugWith _ _ act -> localSeqUnlift env $ \unlift -> unlift act
    DebugNS{} -> pass
    DebugWithNS _ _ _ act -> localSeqUnlift env $ \unlift -> unlift act
    LogMsg{} -> pass
    LogMsgNS{} -> pass
    LogWith _ _ _ act -> localSeqUnlift env $ \unlift -> unlift act
    LogWithNS _ _ _ _ act -> localSeqUnlift env $ \unlift -> unlift act
    WithNamespace _ _ act -> localSeqUnlift env $ \unlift -> unlift act

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
