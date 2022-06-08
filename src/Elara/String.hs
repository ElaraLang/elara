module Elara.String where

import Data.Text as T
import Prelude hiding (String)
import Prelude qualified as P

type String = T.Text

fromString :: P.String -> Text
fromString = T.pack