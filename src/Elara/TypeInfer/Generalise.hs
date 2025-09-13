module Elara.TypeInfer.Generalise where

import Data.Generics.Sum (AsAny (_As))
import Data.Set (difference, member)
import Elara.AST.Region
import Elara.Logging
import Elara.TypeInfer.Environment
import Elara.TypeInfer.Ftv
import Elara.TypeInfer.Type

import Effectful
import Effectful.State.Static.Local (State, get)

generalise :: forall r. (StructuredDebug :> r, State (TypeEnvironment SourceRegion) :> r) => Monotype SourceRegion -> Eff r (Polytype SourceRegion)
generalise ty = do
    env <- get @(TypeEnvironment SourceRegion)
    let freeVars = ftv ty
    let envVars = freeVars `difference` ftv env
    let uniVars = envVars ^.. folded % (_As @"UnificationVar")

    let generalized = Forall (monotypeLoc ty) (toList uniVars) (EmptyConstraint $ monotypeLoc ty) ty
    pure generalized

removeSkolems :: Monotype loc -> Monotype loc
removeSkolems ty = do
    let ftvs = ftv ty

    transformOf
        gplate
        ( \case
            TypeVar loc tv@(SkolemVar tv') | tv `member` ftvs -> TypeVar loc (UnificationVar tv')
            other -> other
        )
        ty
