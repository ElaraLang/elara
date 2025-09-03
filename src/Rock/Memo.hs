
{-# LANGUAGE QuantifiedConstraints #-}

module Rock.Memo where

import Control.Concurrent.Lifted
import Control.Exception.Lifted

import Data.Dependent.HashMap (DHashMap)
import Data.Dependent.HashMap qualified as DHashMap
import Data.Foldable
import Data.GADT.Compare (GEq)
import Data.GADT.Show (GShow)
import Data.HashMap.Lazy qualified as HashMap
import Data.Hashable
import Data.IORef.Lifted
import Data.Kind (Type)
import Data.Some
import Data.Typeable
import Effectful (Eff, Effect, IOE, raise, (:>))
import Effectful.Concurrent.MVar.Strict (Concurrent, newEmptyMVar', putMVar', readMVar')
import Effectful.TH (makeEffect)
import Rock
import Rock.MemoE
import Prelude hiding (atomicModifyIORef, newEmptyMVar, newMVar, putMVar, readIORef, readMVar)

-- * Implicit memoisation-

-- | Proof that every key permits IO
class HasIOE f where
    withIOE :: f es a -> (IOE :> es => Eff es a) -> Eff es a

class HasMemoiseE f where
    withMemoiseE :: f es a -> ((Memoise :> es, Concurrent :> es) => Eff es a) -> Eff es a

{- | Remember what @f@ queries have already been performed and their results in
a 'DHashMap', and reuse them if a query is performed again a second time.

The 'DHashMap' should typically not be reused if there has been some change that
might make a query return a different result.
-}
memoise ::
    forall f.
    (forall es. GEq (f es), forall es a. Hashable (f es a), HasMemoiseE f) =>
    Rules f ->
    Rules f
memoise rules (key :: f es a) = withMemoiseE key $ do
    maybeValueVar <- DHashMap.lookup (HideEffects key) <$> getStartedVar
    case maybeValueVar of
        Nothing -> do
            valueVar <- newEmptyMVar'
            join $ modifyStartedVar $ \started ->
                case DHashMap.alterLookup (Just . fromMaybe valueVar) (HideEffects key) started of
                    (Nothing, started') ->
                        ( started'
                        , do
                            value <- rules key
                            putMVar' valueVar value
                            pure value
                        )
                    (Just valueVar', _started') ->
                        (started, readMVar' valueVar')
        Just valueVar ->
            readMVar' valueVar

-- * Explicit memoisation

data MemoQuery f es a where
    MemoQuery :: f es a -> MemoQuery f (IOE : es) a

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

newtype Cyclic f = Cyclic (Some f)
    deriving (Show)

instance (GShow f, Typeable f) => Exception (Cyclic (f :: Type -> Type))

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
    , forall es a. Show (f es a)
    , forall es. GEq (f es)
    , forall es a. Hashable (f es a)
    ) =>
    IORef (DHashMap (HideEffects f) MemoEntry) ->
    IORef (HashMap ThreadId ThreadId) ->
    Rules f ->
    Rules (MemoQuery f)
memoiseWithCycleDetection startedVar depsVar rules = rules'
  where
    rules' (MemoQuery (key :: f es a)) = do
        maybeEntry <- DHashMap.lookup (HideEffects key) <$> readIORef startedVar
        case maybeEntry of
            Nothing -> do
                threadId <- myThreadId
                valueVar <- newEmptyMVar
                waitVar <- newMVar $ Just []
                join $ atomicModifyIORef startedVar $ \started ->
                    case DHashMap.alterLookup (Just . fromMaybe (Started threadId valueVar waitVar)) (HideEffects key) started of
                        (Nothing, started') ->
                            ( started'
                            , ( do
                                    value <- raise $ rules key
                                    join $ modifyMVar waitVar $ \maybeWaitingThreads -> do
                                        case maybeWaitingThreads of
                                            Nothing ->
                                                error "impossible"
                                            Just waitingThreads ->
                                                pure
                                                    ( Nothing
                                                    , atomicModifyIORef depsVar $ \deps ->
                                                        ( flipfoldl' HashMap.delete deps waitingThreads
                                                        , ()
                                                        )
                                                    )
                                    atomicModifyIORef startedVar $ \started'' ->
                                        (DHashMap.insert (HideEffects key) (Done value) started'', ())
                                    putMVar valueVar $ Just value
                                    pure value
                              )
                                `catch` \(e :: Cyclic (HideEffects f)) -> do
                                    atomicModifyIORef startedVar $ \started'' ->
                                        (DHashMap.delete (HideEffects key) started'', ())
                                    putMVar valueVar Nothing
                                    throwIO e
                            )
                        (Just entry, _started') ->
                            (started, waitFor entry)
            Just entry -> waitFor entry
      where
        waitFor entry =
            case entry of
                Started onThread valueVar waitVar -> do
                    threadId <- myThreadId
                    modifyMVar_ waitVar $ \maybeWaitingThreads -> do
                        case maybeWaitingThreads of
                            Nothing ->
                                pure maybeWaitingThreads
                            Just waitingThreads -> do
                                join $ atomicModifyIORef depsVar $ \deps -> do
                                    let deps' = HashMap.insert threadId onThread deps
                                    if detectCycle threadId deps'
                                        then
                                            ( deps
                                            , throwIO $ Cyclic $ Some (HideEffects key)
                                            )
                                        else
                                            ( deps'
                                            , pass
                                            )
                                pure $ Just $ threadId : waitingThreads
                    maybeValue <- readMVar valueVar
                    maybe (rules' (MemoQuery key)) pure maybeValue
                Done value ->
                    pure value

    detectCycle threadId deps =
        go threadId
      where
        go tid =
            case HashMap.lookup tid deps of
                Nothing -> False
                Just dep
                    | dep == threadId -> True
                    | otherwise -> go dep
