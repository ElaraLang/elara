module Elara.ModuleName where

import Elara.Name (Name)

type Raw = Name

data Canonical = Canonical
  { _package :: Name,
    _module :: Raw
  }