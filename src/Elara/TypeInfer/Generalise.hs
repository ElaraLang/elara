module Elara.TypeInfer.Generalise where

import Data.Generics.Sum (AsAny (_As))
import Data.Set (difference, member)
import Effectful
import Effectful.Error.Static (Error)
import Effectful.State.Static.Local (State, get)

import Elara.AST.Region
import Elara.Logging
import Elara.TypeInfer.Environment
import Elara.TypeInfer.Ftv
import Elara.TypeInfer.Type

{- | 'generalise' takes a monotype and returns a polytype that is generalised over all unification variables that are not free in the current type environment,
alongside a substitution that replaces those unification variables with skolem variables.
-}
generalise ::
    forall r.
    ( StructuredDebug :> r
    , Error (InferError SourceRegion) :> r
    , State (TypeEnvironment SourceRegion) :> r
    , State (LocalTypeEnvironment SourceRegion) :> r
    ) =>
    Monotype SourceRegion -> Eff r (Polytype SourceRegion, Substitution SourceRegion)
generalise ty = do
    env <- get @(TypeEnvironment SourceRegion)
    localEnv <- get @(LocalTypeEnvironment SourceRegion)
    let freeVars = ftv ty
    let envVars = freeVars `difference` (ftv env <> ftv localEnv)
    let uniVars = envVars ^.. folded % (_As @"UnificationVar")

    generalised <- schemeOrThrow (closeOver (monotypeLoc ty) (toList uniVars) ty)

    let substPairs = fmap (\uv -> (uv, TypeVar (monotypeLoc ty) (SkolemVar uv) [])) (toList uniVars)

    let astSubst = Substitution (fromList substPairs)

    pure (generalised, astSubst)

removeSkolems :: Generic loc => Monotype loc -> Monotype loc
removeSkolems ty = do
    let ftvs = ftv ty

    transformOf
        plate
        ( \case
            TypeVar loc tv@(SkolemVar tv') tvs | tv `member` ftvs -> TypeVar loc (UnificationVar tv') tvs
            other -> other
        )
        ty
