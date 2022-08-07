{-# OPTIONS_GHC -Wno-deprecations #-}

module Print where

import Debug.Pretty.Simple (pTraceOptM, pTraceShowOptM)
import Text.Pretty.Simple

printColored :: (Show a) => a -> IO ()
printColored = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg

{-# WARNING debugColored "Debug is still in code" #-}
debugColored :: (Show a, Applicative f) => a -> f ()
debugColored = pTraceShowOptM NoCheckColorTty defaultOutputOptionsDarkBg

{-# WARNING debugColoredStr "Debug is still in code" #-}
debugColoredStr :: (Applicative f) => String -> f ()
debugColoredStr = pTraceOptM NoCheckColorTty defaultOutputOptionsDarkBg
