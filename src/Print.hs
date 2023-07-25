{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Print where

import Debug.Pretty.Simple (pTraceOptM, pTraceShowOptM)
import Elara.Data.Pretty
import Prettyprinter.Render.Terminal
import Text.Pretty.Simple (
    CheckColorTty (NoCheckColorTty),
    defaultOutputOptionsDarkBg,
    pPrintOpt,
    pShow,
 )

printColored :: (Show a, MonadIO m) => a -> m ()
printColored = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg

printPretty :: (Pretty a, MonadIO m) => a -> m ()
printPretty p = liftIO (putDoc (pretty p) *> putStrLn "")

showColored :: (Show a, IsString s) => a -> s
showColored = fromString . toString . pShow

showPretty :: (Pretty a) => a -> Text
showPretty = renderStrict . layoutPretty defaultLayoutOptions . pretty

showPrettyUnannotated :: (Pretty a) => a -> Text
showPrettyUnannotated = renderStrict . layoutPretty defaultLayoutOptions . unAnnotate . pretty

{-# WARNING debugColored "Debug is still in code" #-}
debugColored :: (Show a, Applicative f) => a -> f ()
debugColored = pTraceShowOptM NoCheckColorTty defaultOutputOptionsDarkBg

{-# WARNING debugColoredStr "Debug is still in code" #-}
debugColoredStr :: (Applicative f) => String -> f ()
debugColoredStr = pTraceOptM NoCheckColorTty defaultOutputOptionsDarkBg

{-# WARNING debugPretty "debugPretty is still in code" #-}
debugPretty :: (Applicative m, Pretty a) => a -> m ()
debugPretty = traceM . toString . renderStrict . layoutPretty defaultLayoutOptions . pretty

debugWithResult :: (Show a1, Show a2) => a1 -> a2 -> a2
debugWithResult name res = trace (show name <> " -> " <> show res) res
