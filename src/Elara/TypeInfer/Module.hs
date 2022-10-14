module Elara.TypeInfer.Module where

import Control.Monad.RWS (listen)
import Elara.AST.Canonical (CanonicalModule)
import Elara.AST.Typed (TypedModule)
import Elara.Data.Module (Module (Module))
import Elara.TypeInfer.Declaration (addDeclarationStub, inferDeclaration)
import Elara.TypeInfer.Infer (Infer)
import Print (debugColored)

inferModule :: CanonicalModule -> Infer TypedModule
inferModule (Module name imports exposing declarations) = do
    (r, cs) <- listen $ do
        forM_ declarations addDeclarationStub
        declarations' <- forM declarations inferDeclaration

        -- the problem is that the AST is storing types before they've been fully solved, so stuff is weird. fix or else

        pure $ Module name imports exposing declarations'
    debugColored cs
    pure r