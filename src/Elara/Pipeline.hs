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
import Elara.Error (DiagnosticWriter)
import Elara.Logging
import Error.Diagnose (Diagnostic)
import Polysemy (Effect, Embed, InterpreterFor, Members)
import Polysemy.Log (DataLog, interpretDataLog)
import Polysemy.Maybe (MaybeE)
import Print (printPretty)
import System.IO qualified

-- | All stages of a pipeline must be interpreted into this effect stack.
type PipelineResultEff = '[MaybeE, DiagnosticWriter (Doc AnsiStyle), StructuredDebug, Embed IO]

type IsPipeline r = Members PipelineResultEff r

type PipelineRes a = IO (Diagnostic (Doc AnsiStyle), Maybe a)

type family EffectsAsPrefixOf (effects :: [Effect]) (r :: [Effect]) :: [Effect] where
    EffectsAsPrefixOf '[] ys = ys
    EffectsAsPrefixOf (x ': xs) ys = x ': EffectsAsPrefixOf xs ys

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

destroyDataLog :: InterpreterFor (DataLog (Doc AnsiStyle)) r
destroyDataLog = interpretDataLog (const pass)
