module Elara.TypeInfer.Module where

import Elara.AST.Canonical (CanonicalModule)
import Elara.AST.Typed (TypedModule)
import Elara.Data.Module (Module (Module))
import Elara.TypeInfer.Declaration (addDeclarationStub, inferDeclaration)
import Elara.TypeInfer.Infer (Infer, InferState (typeEnv))
import Print (debugColored)

inferModule :: CanonicalModule -> Infer TypedModule
inferModule (Module name imports exposing declarations) = do
    forM_ declarations addDeclarationStub
    declarations' <- forM declarations inferDeclaration
    pure $ Module name imports exposing declarations'
