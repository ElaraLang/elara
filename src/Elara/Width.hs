{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Elara.Width where

#if !defined(wasm32_HOST_ARCH)
import System.Console.Terminal.Size (Window (..))

import System.Console.Terminal.Size qualified as Size
#endif

-- | Get the width of the terminal (in columns)
getWidth :: IO Int
getWidth = do
#if defined(wasm32_HOST_ARCH)
    pure defaultWidth
#else
    maybeWindow <- Size.size

    let renderWidth =
            case maybeWindow of
                Nothing -> defaultWidth
                Just Window{..} -> width

    pure renderWidth
#endif

-- | The default width to use
defaultWidth :: Int
defaultWidth = 80
