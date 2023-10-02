{-# LANGUAGE RecordWildCards #-}

module Elara.Width where

import System.Console.Terminal.Size (Window (..))
import System.Console.Terminal.Size qualified as Size

-- | Get the width of the terminal (in columns)
getWidth :: IO Int
getWidth = do
    maybeWindow <- Size.size

    let renderWidth =
            case maybeWindow of
                Nothing -> defaultWidth
                Just Window{..} -> width

    pure renderWidth

-- | The default width to use
defaultWidth :: Int
defaultWidth = 80
