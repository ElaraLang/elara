{-# LANGUAGE GADTs #-}

{- | A minimal co-log 'Log' effect for effectful.
Replaces the @co-log-effectful@ package, which cannot be cross-compiled :(
-}
module Effectful.Colog (
    Log,
    logMsg,
    runLogAction,
    module Colog.Core.Action,
) where

import Colog.Core.Action
import Data.Kind (Type)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)

-- | Log messages of type @msg@
data Log (msg :: Type) :: Effect where
    LogMsg :: msg -> Log msg m ()

type instance DispatchOf (Log msg) = Dynamic

-- | Log a message
logMsg :: Log msg :> es => msg -> Eff es ()
logMsg = send . LogMsg

-- | Handle 'Log' by running each message through a 'LogAction'
runLogAction :: LogAction (Eff es) msg -> Eff (Log msg : es) a -> Eff es a
runLogAction (LogAction act) = interpret $ \_ (LogMsg msg) -> act msg
