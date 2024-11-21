-- | Types used by the type inference engine

module Elara.TypeInfer.Type where

-- | A type scheme σ
data Type loc
 deriving (Generic, Show, Eq, Ord)