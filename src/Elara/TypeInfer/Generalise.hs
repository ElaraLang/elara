module Elara.TypeInfer.Generalise where

import Data.Generics.Sum (AsAny (_As))
import Data.Set (difference, member)
import Elara.AST.Region
import Elara.Data.Pretty
import Elara.Logging
import Elara.TypeInfer.Ftv
import Elara.TypeInfer.Monad
import Elara.TypeInfer.Type
import Polysemy hiding (transform)
import Polysemy.State
import Elara.TypeInfer.Environment

generalise :: forall r. Infer SourceRegion r => Monotype SourceRegion -> Sem r (Polytype SourceRegion)
generalise ty = do
    env <- get @(TypeEnvironment SourceRegion)
    let freeVars = ftv ty
    debug $ "Free vars: " <> pretty freeVars <> " in " <> pretty ty
    debug $ "env: " <> pretty env
    debug $ "ftv env: " <> pretty (ftv env)
    let envVars = freeVars `difference` ftv env
    debug $ "envVars: " <> pretty envVars
    let uniVars = envVars ^.. folded % (_As @"UnificationVar")

    let generalized = Forall (toList uniVars) EmptyConstraint ty
    pure generalized

removeSkolems ty = do
    let ftvs = ftv ty

    transformOf
        gplate
        ( \case
            TypeVar tv@(SkolemVar tv') | tv `member` ftvs -> TypeVar (UnificationVar tv')
            other -> other
        )
        ty
