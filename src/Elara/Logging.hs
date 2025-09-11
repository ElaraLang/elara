{-# LANGUAGE AllowAmbiguousTypes #-}

module Elara.Logging where

import Data.Text qualified as T
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, (:>))
import Effectful.Colog qualified as Log
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift, reinterpret, send)
import Effectful.State.Static.Local qualified as S
import Elara.Data.Pretty
import GHC.Exts
import GHC.TypeLits (KnownSymbol (..), symbolVal)

-- type DebugLog = Log.DataLog (Doc AnsiStyle)

data StructuredDebug :: Effect where
    Debug :: HasCallStack => Doc AnsiStyle -> StructuredDebug m ()
    DebugWith :: HasCallStack => Doc AnsiStyle -> m a -> StructuredDebug m a
    DebugNS :: HasCallStack => [T.Text] -> Doc AnsiStyle -> StructuredDebug m ()
    DebugWithNS :: HasCallStack => [T.Text] -> Doc AnsiStyle -> m a -> StructuredDebug m a

type instance DispatchOf StructuredDebug = 'Dynamic

debug :: HasCallStack => StructuredDebug :> r => Doc AnsiStyle -> Eff r ()
debug msg = send $ Debug msg

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

structuredDebugToLog :: forall r a. HasCallStack => Log.Log (Doc AnsiStyle) :> r => Eff (StructuredDebug : r) a -> Eff r a
structuredDebugToLog = reinterpret (S.evalState ([] :: [T.Text]) . S.evalState (0 :: Int)) $ \env -> \case
    Debug msg -> do
        depth <- S.get
        let prefix = stimes depth "│ "
            indentedMsg = prefix <> hang (2 * depth) msg
        Log.logMsg indentedMsg
    DebugNS names msg -> do
        depth <- S.get @Int
        ns <- S.get @[T.Text]
        let fullNs = ns <> names
            nsDoc = if null fullNs then mempty else pretty ("[" <> T.intercalate "." fullNs <> "] ")
            prefix = stimes depth "│ "
            indentedMsg = prefix <> nsDoc <> hang (2 * depth) msg
        Log.logMsg indentedMsg
    DebugWith msg act -> do
        depth <- S.get
        let prefix = stimes depth "│ "
            indentedMsg = prefix <> hang (2 * depth) msg
        Log.logMsg indentedMsg
        S.put (depth + 1)
        res <- localSeqUnlift env $ \unlift -> unlift act
        S.put depth
        pure res
    DebugWithNS names msg act -> do
        depth <- S.get @Int
        ns <- S.get @[T.Text]
        let fullNs = ns <> names
            nsDoc = if null fullNs then mempty else pretty ("[" <> T.intercalate "." fullNs <> "] ")
            prefix = stimes depth "│ "
            indentedMsg = prefix <> nsDoc <> hang (2 * depth) msg
        Log.logMsg indentedMsg
        S.put (depth + 1)
        -- push namespace
        S.put fullNs
        res <- localSeqUnlift env $ \unlift -> unlift act
        -- restore depth and namespace
        S.put depth
        S.put ns
        pure res

ignoreStructuredDebug :: Eff (StructuredDebug : r) a -> Eff r a
ignoreStructuredDebug = interpret $ \env -> \case
    Debug _ -> pass
    DebugWith _ act -> localSeqUnlift env $ \unlift -> unlift act
    DebugNS _ _ -> pass
    DebugWithNS _ _ act -> localSeqUnlift env $ \unlift -> unlift act

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
