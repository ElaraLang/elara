{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Rock.Memo (memoise, memoiseWithCycleDetection, withoutMemoisation, memoiseExplicit) where

import Control.Concurrent.Lifted
import Control.Exception.Lifted

import Data.Dependent.HashMap (DHashMap)
import Data.Dependent.HashMap qualified as DHashMap
import Data.GADT.Compare (GEq)
import Data.HashMap.Lazy qualified as HashMap
import Data.Hashable
import Data.IORef.Lifted
import Data.List (isSuffixOf)
import Data.Some
import Data.Typeable
import Effectful (Eff, IOE, raise, (:>))
import Effectful.Internal.Monad (unEff, unsafeEff, unsafeEff_)
import Elara.Data.Pretty
import Elara.Logging (StructuredDebug, logDebugNS)
import Rock
import Text.Show (Show (..))
import Prelude hiding (atomicModifyIORef, atomicModifyIORef', newEmptyMVar, newMVar, putMVar, readIORef, readMVar)

-- * Implicit memoisation-

{- | Remember what @f@ queries have already been performed and their results in
a 'DHashMap', and reuse them if a query is performed again a second time.

The 'DHashMap' should typically not be reused if there has been some change that
might make a query return a different result.
-}
memoise ::
    forall f.
    ( forall es. GEq (f es)
    , forall es a. Hashable (f es a)
    , HasCallStack
    ) =>
    IORef (DHashMap (HideEffects f) MVar) ->
    Rules f ->
    Rules f
memoise startedVar rules (key :: f es a) = do
    maybeValueVar <- unsafeEff_ (DHashMap.lookup (HideEffects key) <$> readIORef startedVar)
    case maybeValueVar of
        Nothing -> do
            valueVar <- unsafeEff_ newEmptyMVar
            join $ unsafeEff_ $ atomicModifyIORef' startedVar $ \started ->
                case DHashMap.alterLookup (Just . fromMaybe valueVar) (HideEffects key) started of
                    (Nothing, started') ->
                        ( started'
                        , do
                            value <- rules key
                            unsafeEff_ (putMVar valueVar value)
                            pure value
                        )
                    (Just valueVar', _started') ->
                        (started, unsafeEff_ (readMVar valueVar'))
        Just valueVar ->
            unsafeEff_ (readMVar valueVar)

-- * Explicit memoisation

data MemoQuery f es a where
    MemoQuery :: StructuredDebug :> es => f es a -> MemoQuery f (IOE : es) a

-- Don't actually memoise anything
withoutMemoisation :: Rules f -> Rules (MemoQuery f)
withoutMemoisation r (MemoQuery key) = raise $ r key

{- | Remember what @f@ queries have already been performed and their results in
a 'DHashMap', and reuse them if a query is performed again a second time.

The 'DHashMap' should typically not be reused if there has been some change that
might make a query return a different result.
-}
memoiseExplicit ::
    forall f.
    (forall es. GEq (f es), forall es a. Hashable (f es a)) =>
    IORef (DHashMap (HideEffects f) MVar) ->
    Rules f ->
    Rules (MemoQuery f)
memoiseExplicit startedVar rules (MemoQuery (key :: f es a)) = do
    maybeValueVar <- DHashMap.lookup (HideEffects key) <$> readIORef startedVar
    case maybeValueVar of
        Nothing -> do
            valueVar <- newEmptyMVar
            join $ atomicModifyIORef startedVar $ \started ->
                case DHashMap.alterLookup (Just . fromMaybe valueVar) (HideEffects key) started of
                    (Nothing, started') ->
                        ( started'
                        , do
                            value <- raise $ rules key
                            putMVar valueVar value
                            pure value
                        )
                    (Just valueVar', _started') ->
                        (started, readMVar valueVar')
        Just valueVar ->
            readMVar valueVar

-- | Holds the list of keys involved in the cycle, i.e. a call stack of queries
newtype Cyclic f = Cyclic (NonEmpty (Some (HideEffects f)))

instance (Typeable f, forall es a. Show (f es a)) => Exception (Cyclic f)

instance (forall es a. Show (f es a)) => Show (Cyclic f) where
    show (Cyclic chain) =
        "Cyclic dependency detected:\n"
            <> toString (unlines $ toText <$> zipWith formatItem (True : repeat False) (toList chainWithLoop))
      where
        -- Repeat the first element at the end to visually close the loop
        chainWithLoop = chain <> pure (head chain)

        showInner :: (forall es a. Show (f es a)) => Some (HideEffects f) -> String
        showInner (Some (HideEffects k)) = Prelude.show k

        formatItem isFirst item =
            let
                raw :: String = showInner item

                clean =
                    if "HideEffects (" `isPrefixOf` raw && ")" `isSuffixOf` raw
                        then take (length raw - 14) (drop 13 raw)
                        else raw

                prefix = if isFirst then "     " else "  -> "
             in
                prefix <> clean

data MemoEntry a
    = Started !ThreadId !(MVar (Maybe a)) !(MVar (Maybe [ThreadId]))
    | Done !a

{- | Like 'memoise', but throw @'Cyclic' f@ if a query depends on itself, directly or
indirectly.

The 'HashMap' represents dependencies between threads and should not be
reused between invocations.
-}
memoiseWithCycleDetection ::
    forall f.
    ( Typeable f
    , forall es a. (Show (f es a))
    , forall es. GEq (f es)
    , forall es a. Hashable (f es a)
    , forall es a. Show (f es a)
    , HasCallStack
    ) =>
    -- | started queries
    IORef (DHashMap (HideEffects f) MemoEntry) ->
    -- | dependencies between threads
    IORef (HashMap ThreadId ThreadId) ->
    Rules f ->
    Rules f
memoiseWithCycleDetection startedVar depsVar rules = rules'
  where
    rules' key = do
        logDebugNS ["Query"] $ "MemoiseWithCycleDetection: querying " <> pretty (Text.Show.show key)
        maybeEntry <- DHashMap.lookup (HideEffects key) <$> unsafeEff_ (readIORef startedVar)
        case maybeEntry of
            Nothing -> do
                threadId <- unsafeEff_ myThreadId
                valueVar <- unsafeEff_ newEmptyMVar
                waitVar <- unsafeEff_ (newMVar $ Just [])
                join $ unsafeEff_ $ atomicModifyIORef startedVar $ \started ->
                    case DHashMap.alterLookup (Just . fromMaybe (Started threadId valueVar waitVar)) (HideEffects key) started of
                        (Nothing, started') ->
                            ( started'
                            , runNewComputation threadId valueVar waitVar
                            )
                        (Just entry, _started') ->
                            (started, waitFor entry)
            Just entry -> waitFor entry
      where
        runNewComputation _threadId valueVar waitVar = withFrozenCallStack $ do
            value <-
                unsafeCatch (rules key) $ \(e :: Cyclic f) -> do
                    unsafeEff_ $ do
                        atomicModifyIORef' startedVar $ \started'' ->
                            (DHashMap.delete (HideEffects key) started'', ())
                        putMVar valueVar Nothing
                        throwIO e

            -- Cleanup waiting threads
            unsafeEff_ $ do
                modifyMVar_ waitVar $ \case
                    Nothing -> error "impossible: waitVar empty"
                    Just waitingThreads -> do
                        -- Remove dependencies for all waiting threads
                        atomicModifyIORef' depsVar $ \deps ->
                            (flipfoldl' HashMap.delete deps waitingThreads, ())
                        pure Nothing

                -- Mark as Done
                atomicModifyIORef' startedVar $ \started'' ->
                    (DHashMap.insert (HideEffects key) (Done value) started'', ())

                putMVar valueVar $ Just value

            pure value
        waitFor entry = do
            case entry of
                Started onThread valueVar waitVar -> do
                    threadId <- unsafeEff_ myThreadId
                    unsafeEff_ $ modifyMVar_ waitVar $ \maybeWaitingThreads -> do
                        case maybeWaitingThreads of
                            Nothing ->
                                pure maybeWaitingThreads
                            Just waitingThreads -> do
                                join $ atomicModifyIORef depsVar $ \deps -> do
                                    let deps' = HashMap.insert threadId onThread deps
                                    if detectCycle threadId deps'
                                        then
                                            ( deps
                                            , do
                                                let cycleThreads = recoverCyclePath threadId deps'
                                                started <- readIORef startedVar
                                                let threadToKeys = buildThreadKeyMap started
                                                let cycleKeys =
                                                        concatMap
                                                            (\t -> fromMaybe [] (HashMap.lookup t threadToKeys))
                                                            cycleThreads
                                                case nonEmpty cycleKeys of
                                                    Nothing -> error "Impossible: Cycle detected but no keys found in map"
                                                    Just neKeys -> throwIO $ Cyclic neKeys
                                            )
                                        else
                                            ( deps'
                                            , pass
                                            )
                                pure $ Just $ threadId : waitingThreads
                    maybeValue <- unsafeEff_ (readMVar valueVar)
                    maybe (rules' key) pure maybeValue
                Done value ->
                    pure value

    recoverCyclePath :: ThreadId -> HashMap.HashMap ThreadId ThreadId -> [ThreadId]
    recoverCyclePath start deps = go start []
      where
        go curr visited
            | curr `elem` visited = reverse (curr : takeWhile (/= curr) visited)
            | otherwise = case HashMap.lookup curr deps of
                Nothing -> [] -- should be impossible
                Just next -> go next (curr : visited)

    buildThreadKeyMap :: DHashMap (HideEffects f) MemoEntry -> HashMap.HashMap ThreadId [Some (HideEffects f)]
    buildThreadKeyMap =
        DHashMap.foldrWithKey
            ( \k v acc -> case v of
                Started tid _ _ -> HashMap.insertWith (<>) tid [Some k] acc
                _ -> acc
            )
            HashMap.empty

    detectCycle threadId deps =
        go threadId
      where
        go tid =
            case HashMap.lookup tid deps of
                Nothing -> False
                Just dep
                    | dep == threadId -> True
                    | otherwise -> go dep

{- | Internal helper to catch exceptions without requiring 'IOE'.
This is safe here because we are building infrastructure that we know runs in IO,
but we don't want to leak that implementation detail to the type signature, as then
every query could perform IO actions.
-}
unsafeCatch :: Exception e => Eff es a -> (e -> Eff es a) -> Eff es a
unsafeCatch m handler = unsafeEff $ \env ->
    unEff m env `catch` \e -> unEff (handler e) env
