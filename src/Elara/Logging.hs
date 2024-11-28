module Elara.Logging where

import Data.Text qualified as Text
import Polysemy
import Polysemy.Log qualified as Log
import Polysemy.State (State, evalState, get, put)
import Print (elaraDebug)
import Elara.Data.Pretty

data StructuredDebug m a where
    Debug :: HasCallStack => Doc AnsiStyle -> StructuredDebug m ()
    DebugWith :: HasCallStack => Doc AnsiStyle -> m a -> StructuredDebug m a

debug :: HasCallStack => Member StructuredDebug r => Doc AnsiStyle -> Sem r ()
debug msg = send $ Debug msg

debugWith :: HasCallStack => Member StructuredDebug r => Doc AnsiStyle -> Sem r a -> Sem r a
debugWith msg act = send $ DebugWith msg act

structuredDebugToLog :: forall r a. Member Log.Log r => Sem (StructuredDebug : r) a -> Sem r a
structuredDebugToLog =
    if not elaraDebug
        then raise_ . interpretH (\case Debug _ -> pureT (); DebugWith _ act -> runTSimple act)
        else
            subsume_
                . evalState 0
                . reinterpret2H @StructuredDebug @(State Int) @Log.Log @r
                    ( \case
                        Debug msg -> do
                            depth <- get
                            Log.debug $ Text.replicate depth "│ " <> prettyToText msg
                            pureT ()
                        DebugWith msg act -> do
                            depth <- get
                            Log.debug $ Text.replicate depth "│ " <> prettyToText msg
                            put $ depth + 1
                            a <- runTSimple act
                            put depth
                            pure a
                    )


ignoreStructuredDebug :: Sem (StructuredDebug : r) a -> Sem r a
ignoreStructuredDebug = interpretH $ \case
    Debug _ -> pureT ()
    DebugWith _ act -> runTSimple act