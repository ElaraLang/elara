{- | Stores information about the primitive functions of Elara. These are still written in the source code, with a special name and value.
 The compiler will then replace these with the actual primitive functions.
-}
module Elara.Prim where

println :: Text
println = "elara_prim_println"