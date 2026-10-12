{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- Modified version of the Rock (https://hackage.haskell.org/package/rock) library
-- with effectful support added
-- Credit to https://gist.github.com/expipiplus1/cfd5c4fb4a5a40338ccf8642fb3d0f1e for the initial changes,
-- further modifications Copyright (c) Alexander Wood, MIT.
module Rock.Memo (
    memoiseWithCycleDetection,
    tracingRules,
) where

import Control.Concurrent (ThreadId, myThreadId)
import Data.Dependent.HashMap (DHashMap)
import Data.GADT.Compare (GEq)
import Data.Hashable
import Data.Kind (Type)
import Data.List.NonEmpty ((<|))
import Data.Some
import Data.Typeable
import Effectful.Internal.Monad (unEff, unsafeEff, unsafeEff_)
import Text.Show (Show (..))
import Unsafe.Coerce (unsafeCoerce)

import Control.Concurrent.MVar qualified as MVar
import Control.Exception qualified as E
import Data.Dependent.HashMap qualified as DHashMap
import Data.HashMap.Lazy qualified as HashMap
import Data.List qualified as List
import Data.Set qualified as Set

import Elara.Data.Pretty
import Rock
import Prelude

{- | Force the contents of a 'DHashMap' to be evaluated to normal form.
Not sure if this is actually necessary, but there aren't really any benefits to keeping it lazy
-}
forceDHashMap :: DHashMap k v -> IO (DHashMap k v)
forceDHashMap m = evaluateWHNF (DHashMap.size m `seq` m)

-- | exception type for reporting cyclic dependencies
newtype Cyclic (f :: Type -> Type)
    = -- | the chain of dependencies leading to the cycle
      Cyclic
        (NonEmpty (Some f))

instance (Typeable f, forall a. Show (f a), GEq f) => Exception (Cyclic f)

instance (forall a. Show (f a), GEq f) => Show (Cyclic f) where
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
        showInner :: (forall a. Show (f a)) => Some f -> String
        showInner (Some k) = Prelude.show k

        formatItem :: Text -> Some f -> Text
        formatItem prefix item =
            prefix <> toText (showInner item)

        formatCycleItem :: Bool -> Some f -> Text
        formatCycleItem isFirst item =
            let prefix = if isFirst then "  ┌> " else "  │  "
             in prefix <> toText (showInner item)

data CycleQueryOutcome a
    = CycleSucceeded !a
    | CycleFailed !E.SomeException

data MemoEntry a
    = Started !ThreadId !(MVar.MVar (CycleQueryOutcome a))
    | Done !a

memoiseWithCycleDetection ::
    forall f.
    ( Typeable f
    , forall a. Show (f a)
    , GEq f
    , forall a. Hashable (f a)
    , HasCallStack
    ) =>
    MVar.MVar (DHashMap f MemoEntry) ->
    IORef (HashMap ThreadId ThreadId) ->
    Rules f ->
    Rules f
memoiseWithCycleDetection startedVar depsVar rules = withFrozenCallStack rules'
  where
    rules' key = do
        let !hk = key
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
                            result <- E.tryWithContext $ restore $ unEff (rules key) env
                            case result of
                                Left exWithCtx@(E.ExceptionWithContext _ctx ex)
                                    | Just (Cyclic childTrace) <- fromException ex -> do
                                        let cycleEx = Cyclic (Some hk <| childTrace)
                                        putMVar valueVar (CycleFailed (toException cycleEx))
                                        E.throwIO cycleEx
                                    | otherwise -> do
                                        putMVar valueVar (CycleFailed ex)
                                        E.rethrowIO exWithCtx
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
                                                        E.throwIO $ Cyclic (Some key :| [k])
                                                    Nothing ->
                                                        E.throwIO $ Cyclic (Some key :| [])
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

    findKeyByEntry :: MemoEntry a -> DHashMap f MemoEntry -> Maybe (Some f)
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

{- | Wrap a 'Rules' computation to record query dependency edges.

Each time query @child@ is fetched while query @parent@ is being evaluated,
the edge @(parent, child)@ is written into @edgesVar@. Node names are the
first word of @show key@, i.e. the constructor name without arguments.

This allows us to reconstruct the call graph of queries after execution, for debugging or visualisation.
@
-}
tracingRules ::
    forall f.
    (forall a. Show (f a)) =>
    -- | Thread-local map of which query is currently being evaluated on each thread
    IORef (HashMap.HashMap ThreadId String) ->
    -- | Accumulated dependency edges
    IORef (Set (String, String)) ->
    Rules f ->
    Rules f
tracingRules currentVar edgesVar rules key = do
    let keyName = case List.words (Prelude.show key) of
            (w : _) -> w
            [] -> Prelude.show key
    threadId <- unsafeEff_ myThreadId
    maybeParent <- unsafeEff_ $ HashMap.lookup threadId <$> readIORef currentVar
    case maybeParent of
        Just parent ->
            unsafeEff_ $ atomicModifyIORef' edgesVar $ \edges ->
                (Set.insert (parent, keyName) edges, ())
        Nothing -> pass
    unsafeEff $ \env -> do
        prev <-
            atomicModifyIORef' currentVar $ \m ->
                (HashMap.insert threadId keyName m, HashMap.lookup threadId m)
        let restoreVar =
                atomicModifyIORef' currentVar $ \m ->
                    ( case prev of
                        Just p -> HashMap.insert threadId p m
                        Nothing -> HashMap.delete threadId m
                    , ()
                    )
        E.finally (unEff (rules key) env) restoreVar
