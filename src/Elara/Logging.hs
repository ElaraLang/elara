module Elara.Logging where

import Elara.Data.Pretty
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
