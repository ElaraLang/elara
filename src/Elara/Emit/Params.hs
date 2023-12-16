module Elara.Emit.Params where

newtype GenParams = GenParams
    { checkCasts :: Bool
    -- ^ Whether to insert checkcasts
    }
    deriving (Eq, Show)

defaultGenParams :: GenParams
defaultGenParams = GenParams True
