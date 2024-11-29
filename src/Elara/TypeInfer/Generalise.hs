module Elara.TypeInfer.Generalise where
import Elara.TypeInfer.Monad
import Elara.AST.Region
import Elara.TypeInfer.Type
import Polysemy
import Elara.TypeInfer.Ftv
import Polysemy.State
import Data.Set (difference)
import Elara.Logging
import Elara.Data.Pretty

generalise :: forall r. Infer SourceRegion r => Monotype SourceRegion -> Sem r (Polytype SourceRegion)
generalise ty = do
    env <- get
    let freeVars = ftv ty
    debug $ "Free vars: " <> pretty freeVars <> " in " <> pretty ty
    debug $ "env: " <> pretty env
    debug $ "ftv env: " <> pretty (ftv env)
    let envVars = freeVars `difference` ftv env
    
    
    let generalized = Forall (toList envVars) EmptyConstraint ty
    pure generalized