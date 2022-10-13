module Elara.TypeInfer.Module where

import Elara.AST.Canonical (CanonicalModule)
import Elara.AST.Typed (TypedModule)
import Elara.Data.Module (Module (Module))
import Elara.TypeInfer.Declaration (inferDeclaration)
import Elara.TypeInfer.Infer (Infer)

inferModule :: CanonicalModule -> Infer TypedModule
inferModule (Module name imports exposing declarations) = do
    declarations' <- forM declarations inferDeclaration
    pure $ Module name imports exposing declarations'
