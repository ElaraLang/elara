{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Print where

import Text.Pretty.Simple (
    pShow,
 )

import Elara.Data.Pretty

import Elara.Width qualified as Width

elaraDebug :: Bool
elaraDebug = False
{-# INLINE elaraDebug #-}

printPretty :: (Pretty a, MonadIO m) => a -> m ()
printPretty p = liftIO $ do
    width <- Width.getWidth
    putTextLn $ renderStrict True width (pretty p)

showColored :: (Show a, IsString s) => a -> s
showColored = fromString . toString . pShow

showPretty :: Pretty a => a -> Text
showPretty = prettyToText

prettyToString :: Pretty a => a -> String
prettyToString = toString . prettyToText

showPrettyUnannotated :: Pretty a => a -> Text
showPrettyUnannotated = prettyToUnannotatedText

debugWithResult :: (Show a1, Show a2) => a1 -> a2 -> a2
debugWithResult name res = trace (show name <> " -> " <> show res) res
