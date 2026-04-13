{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Rock.Memo (memoise, memoiseWithCycleDetection, withoutMemoisation, memoiseExplicit) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.MVar qualified as MVar
import Control.Exception qualified as E
import Data.Dependent.HashMap (DHashMap)
import Data.Dependent.HashMap qualified as DHashMap
import Data.GADT.Compare (GEq)
import Data.HashMap.Lazy qualified as HashMap
import Data.Hashable
import Data.List (isSuffixOf)
import Data.List.NonEmpty ((<|))
import Data.Some
import Data.Typeable
import Effectful (IOE, raise, (:>))
import Effectful.Internal.Monad (unEff, unsafeEff, unsafeEff_)
import Elara.Data.Pretty
import Elara.Logging (StructuredDebug)
import Rock
import Text.Show (Show (..))
import Unsafe.Coerce (unsafeCoerce)
import Prelude

{- | Force the contents of a 'DHashMap' to be evaluated to normal form.
Not sure if this is actually necessary, but there aren't really any benefits to keeping it lazy
-}
forceDHashMap :: DHashMap k v -> IO (DHashMap k v)
forceDHashMap m = evaluateWHNF (DHashMap.size m `seq` m)

{- | The result of a memoised query. Exceptions are caught on the computing thread
and stored here, then rethrown on waiting threads. This ensures each thread encounters
the exception in its own context, preventing thread-bound effect issues.
-}
data QueryResult a = Succeeded !a | Failed !E.SomeException

-- | A variable holding the result of a memoised query, allowing waiting threads to block until the result is available
newtype ResultVar a = ResultVar (MVar.MVar (QueryResult a))

-- * Implicit memoisation

memoise ::
    forall f.
    ( forall es. GEq (f es)
    , forall es a. Hashable (f es a)
    , HasCallStack
    ) =>
    MVar.MVar (DHashMap (HideEffects f) ResultVar) ->
    Rules f ->
    Rules f
memoise startedVar rules = rules'
  where
    rules' (key :: f es a) = do
        let !hk = HideEffects key
        maybeValueVar <- unsafeEff_ (DHashMap.lookup hk <$> readMVar startedVar)
        case maybeValueVar of
            Just (ResultVar valueVar) -> do
                res <- unsafeEff_ (readMVar valueVar)
                case res of
                    Succeeded v -> pure v
                    Failed ex -> unsafeEff_ (E.throwIO ex)
            Nothing -> do
                valueVar <- unsafeEff_ newEmptyMVar
                let resVar = ResultVar valueVar
                unsafeEff $ \env -> E.mask $ \restore -> do
                    action <- MVar.modifyMVar startedVar $ \started ->
                        case DHashMap.lookup hk started of
                            Just valueVar' ->
                                pure (started, Right valueVar')
                            Nothing -> do
                                started' <- forceDHashMap $ DHashMap.insert hk resVar started
                                pure (started', Left ())
                    case action of
                        Right (ResultVar valueVar') -> do
                            res <- restore (readMVar valueVar')
                            case res of
                                Succeeded v -> pure v
                                Failed ex -> E.throwIO ex
                        Left () -> do
                            result <- E.try $ restore $ unEff (rules key) env
                            case result of
                                Right value -> do
                                    putMVar valueVar (Succeeded value)
                                    pure value
                                Left (ex :: E.SomeException) -> do
                                    putMVar valueVar (Failed ex)
                                    E.throwIO ex

-- * Explicit memoisation

data MemoQuery f es a where
    MemoQuery :: StructuredDebug :> es => f es a -> MemoQuery f (IOE : es) a

withoutMemoisation :: Rules f -> Rules (MemoQuery f)
withoutMemoisation r (MemoQuery key) = raise $ r key

memoiseExplicit ::
    forall f.
    (forall es. GEq (f es), forall es a. Hashable (f es a)) =>
    MVar.MVar (DHashMap (HideEffects f) ResultVar) ->
    Rules f ->
    Rules (MemoQuery f)
memoiseExplicit startedVar rules = rules'
  where
    rules' (MemoQuery (key :: f es a)) = do
        let !hk = HideEffects key
        maybeValueVar <- unsafeEff_ (DHashMap.lookup hk <$> readMVar startedVar)
        case maybeValueVar of
            Just (ResultVar valueVar) -> do
                res <- unsafeEff_ (readMVar valueVar)
                case res of
                    Succeeded v -> pure v
                    Failed ex -> unsafeEff_ (E.throwIO ex)
            Nothing -> do
                valueVar <- unsafeEff_ newEmptyMVar
                let resVar = ResultVar valueVar
                unsafeEff $ \env -> E.mask $ \restore -> do
                    action <- MVar.modifyMVar startedVar $ \started ->
                        case DHashMap.lookup hk started of
                            Just valueVar' ->
                                pure (started, Right valueVar')
                            Nothing -> do
                                started' <- forceDHashMap $ DHashMap.insert hk resVar started
                                pure (started', Left ())
                    case action of
                        Right (ResultVar valueVar') -> do
                            res <- restore (readMVar valueVar')
                            case res of
                                Succeeded v -> pure v
                                Failed ex -> E.throwIO ex
                        Left () -> do
                            result <- E.try $ restore $ unEff (raise $ rules key) env
                            case result of
                                Right value -> do
                                    putMVar valueVar (Succeeded value)
                                    pure value
                                Left (ex :: E.SomeException) -> do
                                    putMVar valueVar (Failed ex)
                                    E.throwIO ex

-- | exception type for reporting cyclic dependencies
newtype Cyclic f
    = -- | the chain of dependencies leading to the cycle
      Cyclic
        (NonEmpty (Some (HideEffects f)))

instance (Typeable f, forall es a. Show (f es a), forall es. GEq (f es)) => Exception (Cyclic f)

instance (forall es a. Show (f es a), forall es. GEq (f es)) => Show (Cyclic f) where
    show (Cyclic chain) =
        let traceList = toList chain
            lastItem = last chain
            knotIndex = case break (== lastItem) (init chain) of
                (_, []) -> Nothing
                (prefix, _) -> Just (length prefix)
         in case knotIndex of
                Nothing ->
                    "Cyclic dependency detected:\n" <> toString (unlines (map (formatItem "  -> ") traceList))
                Just idx ->
                    let (stem, cyclePart) = splitAt idx (init chain)
                     in "Cyclic dependency detected:\n"
                            <> (if null stem then "" else "Path to cycle:\n")
                            <> toString (unlines (map (formatItem "     ") stem))
                            <> "The Cycle:\n"
                            <> toString (unlines (zipWith formatCycleItem (True : repeat False) (cyclePart <> [lastItem])))
      where
        showInner :: (forall es a. Show (f es a)) => Some (HideEffects f) -> String
        showInner (Some (HideEffects k)) = Prelude.show k

        cleanStr :: String -> String
        cleanStr raw =
            toString
                ( if "HideEffects (" `isPrefixOf` raw && ")" `isSuffixOf` raw
                    then take (length raw - 14) (drop 13 raw)
                    else raw
                )

        formatItem :: Text -> Some (HideEffects f) -> Text
        formatItem prefix item =
            prefix <> toText (cleanStr (showInner item))

        formatCycleItem :: Bool -> Some (HideEffects f) -> Text
        formatCycleItem isFirst item =
            let prefix = if isFirst then "  ┌> " else "  │  "
             in prefix <> toText (cleanStr (showInner item))

data CycleQueryOutcome a
    = CycleSucceeded !a
    | CycleFailed !E.SomeException

data MemoEntry a
    = Started !ThreadId !(MVar.MVar (CycleQueryOutcome a))
    | Done !a

memoiseWithCycleDetection ::
    forall f.
    ( Typeable f
    , forall es a. (Show (f es a))
    , forall es. GEq (f es)
    , forall es a. Hashable (f es a)
    , forall es a. Show (f es a)
    , HasCallStack
    ) =>
    MVar.MVar (DHashMap (HideEffects f) MemoEntry) ->
    IORef (HashMap ThreadId ThreadId) ->
    Rules f ->
    Rules f
memoiseWithCycleDetection startedVar depsVar rules = rules'
  where
    rules' key = do
        let !hk = HideEffects key
        maybeEntry <- DHashMap.lookup hk <$> unsafeEff_ (readMVar startedVar)
        case maybeEntry of
            Nothing -> do
                threadId <- unsafeEff_ myThreadId
                valueVar <- unsafeEff_ newEmptyMVar
                unsafeEff $ \env -> E.mask $ \restore -> do
                    action <- MVar.modifyMVar startedVar $ \started ->
                        case DHashMap.lookup hk started of
                            Just existingEntry ->
                                pure (started, Right existingEntry)
                            Nothing -> do
                                let !newEntry = Started threadId valueVar
                                started' <- forceDHashMap $ DHashMap.insert hk newEntry started
                                pure (started', Left ())
                    case action of
                        Right existingEntry ->
                            unEff (waitFor existingEntry) env
                        Left () -> do
                            result <- E.try $ restore $ unEff (rules key) env
                            case (result :: Either E.SomeException _) of
                                Left ex
                                    | Just (Cyclic childTrace) <- fromException ex -> do
                                        let cycleEx = Cyclic (Some hk <| childTrace)
                                        putMVar valueVar (CycleFailed (toException cycleEx))
                                        E.throwIO cycleEx
                                    | otherwise -> do
                                        putMVar valueVar (CycleFailed ex)
                                        E.throwIO ex
                                Right value -> do
                                    putMVar valueVar (CycleSucceeded value)
                                    MVar.modifyMVar_ startedVar $ \started'' -> forceDHashMap $ DHashMap.insert hk (Done value) started''
                                    pure value
            Just entry ->
                waitFor entry
      where
        unregisterWaiter :: ThreadId -> IO ()
        unregisterWaiter threadId = atomicModifyIORef' depsVar $ \deps ->
            (HashMap.delete threadId deps, ())

        waitFor entry = case entry of
            Started onThread valueVar -> do
                alreadyDone <- unsafeEff_ $ tryReadMVar valueVar
                case alreadyDone of
                    Just outcome ->
                        unsafeEff_ (handleOutcome outcome)
                    Nothing ->
                        do
                            unsafeEff_ $ E.mask $ \restore -> do
                                threadId <- myThreadId
                                join $ atomicModifyIORef' depsVar $ \deps -> do
                                    let deps' = HashMap.insert threadId onThread deps
                                    if detectCycle threadId deps'
                                        then
                                            ( deps
                                            , do
                                                started <- readMVar startedVar
                                                let targetKey = findKeyByEntry entry started
                                                case targetKey of
                                                    Just k ->
                                                        E.throwIO $ Cyclic (Some (HideEffects key) :| [k])
                                                    Nothing ->
                                                        E.throwIO $ Cyclic (Some (HideEffects key) :| [])
                                            )
                                        else
                                            ( deps'
                                            , pass
                                            )
                                outcome <-
                                    restore (readMVar valueVar)
                                        `E.onException` unregisterWaiter threadId
                                unregisterWaiter threadId
                                pure outcome
                            >>= unsafeEff_ . handleOutcome
            Done value ->
                pure value

        handleOutcome = \case
            CycleSucceeded v ->
                pure v
            CycleFailed ex ->
                E.throwIO ex

    findKeyByEntry :: MemoEntry a -> DHashMap (HideEffects f) MemoEntry -> Maybe (Some (HideEffects f))
    findKeyByEntry targetEntry =
        DHashMap.foldrWithKey
            ( \k v acc ->
                case (v, targetEntry) of
                    (Started _ vVar1, Started _ vVar2)
                        | unsafeCoerce vVar1 == vVar2 -> Just (Some k)
                    _ -> acc
            )
            Nothing

    detectCycle threadId deps = go threadId []
      where
        go tid visited
            | tid `elem` visited = False -- disjoint cycle, not ours
            | otherwise =
                case HashMap.lookup tid deps of
                    Nothing -> False
                    Just dep
                        | dep == threadId -> True
                        | otherwise -> go dep (tid : visited)
