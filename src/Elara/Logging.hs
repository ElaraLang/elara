{-# LANGUAGE AllowAmbiguousTypes #-}

module Elara.Logging where

import Elara.Data.Pretty
import GHC.Exts
import GHC.TypeLits (KnownSymbol (..), symbolVal)
import Polysemy
import Polysemy.Log qualified as Log
import Polysemy.State (State, evalState, get, put)
import Print (elaraDebug)

type DebugLog = Log.DataLog (Doc AnsiStyle)

data StructuredDebug m a where
    Debug :: HasCallStack => Doc AnsiStyle -> StructuredDebug m ()
    DebugWith :: HasCallStack => Doc AnsiStyle -> m a -> StructuredDebug m a

debug :: HasCallStack => Member StructuredDebug r => Doc AnsiStyle -> Sem r ()
debug msg = send $ Debug msg

debugWith :: HasCallStack => Member StructuredDebug r => Doc AnsiStyle -> Sem r a -> Sem r a
debugWith msg act = send $ DebugWith msg act

debugWithResult :: (Member StructuredDebug r, Pretty a) => Doc AnsiStyle -> Sem r a -> Sem r a
debugWithResult msg act = debugWith msg $ do
    res <- act
    debug ("Result: " <> pretty res)
    pure res

structuredDebugToLog :: forall r a. Member (Log.DataLog (Doc AnsiStyle)) r => Sem (StructuredDebug : r) a -> Sem r a
structuredDebugToLog =
    if not elaraDebug
        then raise_ . interpretH (\case Debug _ -> pureT (); DebugWith _ act -> runTSimple act)
        else
            subsume_
                . evalState 0
                . reinterpret2H @StructuredDebug @(State Int) @(Log.DataLog (Doc AnsiStyle)) @r
                    ( \case
                        Debug msg -> do
                            depth <- get
                            let prefix = stimes depth "│ "
                            let indentedMsg = prefix <> (hang (2 * depth) msg)
                            Log.dataLog $ indentedMsg
                            pureT ()
                        DebugWith msg act -> do
                            depth <- get
                            let prefix = stimes depth "│ "
                            let indentedMsg = prefix <> (hang (2 * depth) msg)
                            Log.dataLog $ indentedMsg
                            put $ depth + 1
                            a <- runTSimple act
                            put depth
                            pure a
                    )

ignoreStructuredDebug :: Sem (StructuredDebug : r) a -> Sem r a
ignoreStructuredDebug = interpretH $ \case
    Debug _ -> pureT ()
    DebugWith _ act -> runTSimple act

{- | Inspired by https://x.com/Quelklef/status/1860188828876583146 !
A recursive, pure function, which can be traced with a monadic effect.
-}
type TraceableFn (name :: Symbol) a b = forall m. Monad m => (a -> m b) -> a -> m b

-- | Purely run a traceable function, without any tracing
runTraceable :: TraceableFn name a b -> a -> b
runTraceable f a = runIdentity $ do
    f' <- f (pure . runTraceable f) a
    pure f'

-- | Run a traceable function with structured debug tracing
traceFn ::
    forall (name :: Symbol) a b r.
    (Pretty a, Pretty b, Member StructuredDebug r, KnownSymbol name) =>
    TraceableFn name a b -> (a -> Sem r b)
traceFn f a = do
    let p = symbolVal (Proxy @name)
    res <- debugWith (pretty p <> ":" <+> pretty a) $ do
        f (traceFn @name @a @b @r f) a
    debug $ pretty res
    pure res

fib :: TraceableFn "fib" Int Int
fib _ 0 = pure 0
fib _ 1 = pure 1
fib f n = do
    a <- f (n - 1)
    b <- f (n - 2)
    pure $ a + b

x :: Sem '[StructuredDebug] Int
x = traceFn @"fib" fib 10
