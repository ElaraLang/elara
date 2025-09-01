module Elara.TypeInfer.Monad where

import Elara.Data.Unique
import Elara.Logging
import Elara.TypeInfer.Environment
import Elara.TypeInfer.Type (Constraint)
import Polysemy
import Polysemy.Error
import Polysemy.State
import Polysemy.Writer

type InferEffects loc =
    '[ Writer (Constraint loc)
     , State (LocalTypeEnvironment loc)
     , State (TypeEnvironment loc)
     , Error (InferError loc)
     , UniqueGen
     , StructuredDebug
     ]

type Infer loc r = Members (InferEffects loc) r
