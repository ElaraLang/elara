module Elara.TypeInfer.Generalise where

import Data.Generics.Sum (AsAny (_As))
import Data.Set (difference)
import Elara.AST.Region
import Elara.Data.Pretty
import Elara.Logging
import Elara.TypeInfer.Ftv
import Elara.TypeInfer.Monad
import Elara.TypeInfer.Type
import Elara.TypeInfer.Unique (UniqueTyVar)
import Optics
import Polysemy
import Polysemy.State

generalise :: forall r. Infer SourceRegion r => Monotype SourceRegion -> Sem r (Polytype SourceRegion)
generalise ty = do
    env <- get
    let freeVars = ftv ty
    debug $ "Free vars: " <> pretty freeVars <> " in " <> pretty ty
    debug $ "env: " <> pretty env
    debug $ "ftv env: " <> pretty (ftv env)
    let envVars = freeVars `difference` ftv env
    let uniVars = envVars ^.. folded % (_As @"UnificationVar")

    let generalized = Forall (toList uniVars) EmptyConstraint ty
    pure generalized
