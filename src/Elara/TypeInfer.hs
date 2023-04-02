module Elara.TypeInfer where

import Elara.AST.Module
import Elara.AST.Select
import Elara.Data.Unique
import Elara.TypeInfer.AssignIds (assignIdsToModule)
import Elara.TypeInfer.Error
import Elara.TypeInfer.Finalise (TVMap, finaliseModule)
import Elara.TypeInfer.GenerateEquations (generateEquationsForModule)
import Elara.TypeInfer.SubstitutionMap
import Elara.TypeInfer.Unify (unifyAllEquations)
import Polysemy hiding (transform)
import Polysemy.Error (Error)
import Polysemy.State
import Polysemy.Writer
import Print

inferModule ::
    ( Member UniqueGen r
    , Member (State TVMap) r
    , Member (Error (TypeError, SubstitutionMap)) r
    , Member (Error TypeError) r
    ) =>
    Module Shunted ->
    Sem r (Module _)
inferModule m = do
    ids <- assignIdsToModule m
    (env, (equations, _)) <- runState mempty $ runWriter $ generateEquationsForModule ids
    substitution <- unifyAllEquations (toList equations) env mempty
    substituted <- substitute substitution ids
    debugColored (let (SubstitutionMap m) = substitution in length m)
    debugPretty substitution

    pure substituted
