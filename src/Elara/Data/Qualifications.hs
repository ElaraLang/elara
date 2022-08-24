module Elara.Data.Qualifications where
import Elara.Data.Name (ModuleName)

type Unqualified = Void

type MaybeQualified = Maybe ModuleName

type Qualified = ModuleName