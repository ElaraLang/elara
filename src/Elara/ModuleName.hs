module Elara.ModuleName where

import Elara.Name (Name)
import Elara.Package qualified as Pkg

type Raw = Name

data Canonical = Canonical
  { _package :: Pkg.Name,
    _module :: Raw
  }