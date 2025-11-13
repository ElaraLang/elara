{-# LANGUAGE ImportQualifiedPost #-}

module Rules where

import Elara.Query
import Rock qualified

testRules :: HasCallStack => Rock.Rules Query
testRules = \case
    -- GetOpInfo op -> runGetOpInfoQuery op
    other -> error "No rule"
