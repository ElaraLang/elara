module Elara.Data.Qualifications where
import Data.Void (Void)
import Elara.Data.Name (ModuleName)

type Unqualified = Void

type MaybeQualified = Maybe ModuleName

type Qualified = ModuleName