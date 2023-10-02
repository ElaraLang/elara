{-# OPTIONS_GHC -Wno-dodgy-exports #-}

module Elara.AST.Generic.Instances (
    module Elara.AST.Generic.Instances.Simple,
    module Elara.AST.Generic.Instances.DataPlated,
    module Elara.AST.Generic.Instances.Pretty,
    module Elara.AST.Generic.Instances.StripLocation,
)
where

import Elara.AST.Generic.Instances.DataPlated ()
import Elara.AST.Generic.Instances.Pretty ()
import Elara.AST.Generic.Instances.Simple ()
import Elara.AST.Generic.Instances.StripLocation ()
