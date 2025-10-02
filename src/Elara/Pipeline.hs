{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Each stage of AST processing can be interpreted as part of an overall pipeline of effects in the 'Sem' monad.
This acts as the entrypoint to the stage, bringing each stage into a common abstraction
-}
module Elara.Pipeline where

import Colog.Core (LogAction (..))
import Data.Text.IO qualified as Text
import Effectful (Eff, IOE, (:>))
import Effectful.Colog (Log, runLogAction)
import Elara.Data.Pretty
import Print (printPretty)
import System.IO qualified

-- Create a co-log LogAction that prints to stdout and appends to a log file.
-- Returns an IO action that constructs the LogAction so callers (e.g. `Main`) can
-- pass it into `runLogAction` from `Effectful.Colog`.
-- An Effectful interpreter for the `Log (Doc AnsiStyle)` effect which
-- writes prettified, annotated output to stdout and appends an unannotated
-- textual form to `elara.log`.
runLogToStdoutAndFile :: IOE :> es => Eff (Log (Doc AnsiStyle) : es) a -> Eff es a
runLogToStdoutAndFile eff = do
    -- reset log file
    liftIO $ writeFileText "elara.log" ""
    handle <- liftIO $ System.IO.openFile "elara.log" WriteMode
    liftIO $ hSetBuffering handle System.IO.LineBuffering
    let la =
            LogAction
                ( \doc ->
                    liftIO $ printPretty doc *> Text.hPutStrLn handle (prettyToUnannotatedText doc)
                )
    runLogAction la eff
