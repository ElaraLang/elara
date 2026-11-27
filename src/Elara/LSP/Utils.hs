module Elara.LSP.Utils where

import Language.LSP.Protocol.Types

maybeToLspInl :: Maybe a -> a |? Null
maybeToLspInl (Just x) = InL x
maybeToLspInl Nothing = InR Null
