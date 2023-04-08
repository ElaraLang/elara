{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Print where

import Data.Text.Prettyprint.Doc.Render.String
import Debug.Pretty.Simple (pTraceOptM, pTraceShowOptM)
import Elara.Data.Pretty
import Text.Pretty.Simple

printColored :: (Show a, MonadIO m) => a -> m ()
printColored = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg

printPretty :: (Pretty a, MonadIO m) => a -> m ()
printPretty = putStrLn . renderString . layoutPretty defaultLayoutOptions . pretty

{-# WARNING debugColored "Debug is still in code" #-}
debugColored :: (Show a, Applicative f) => a -> f ()
debugColored = pTraceShowOptM NoCheckColorTty defaultOutputOptionsDarkBg

{-# WARNING debugColoredStr "Debug is still in code" #-}
debugColoredStr :: (Applicative f) => String -> f ()
debugColoredStr = pTraceOptM NoCheckColorTty defaultOutputOptionsDarkBg

prettyShow :: (Show a, IsString s) => a -> s
prettyShow = fromString . toString . pShow

{-# WARNING debugPretty "debugPretty is still in code" #-}
debugPretty :: (Applicative m, Pretty a) => a -> m ()
debugPretty = traceM . renderString . layoutPretty defaultLayoutOptions . pretty

debugWithResult :: (Show a1, Show a2) => a1 -> a2 -> a2
debugWithResult name res = trace (show name <> " -> " <> show res) res