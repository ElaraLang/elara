module Elara.TypeInfer.Declaration where
import Elara.AST.Canonical (CanonicalDeclaration)
import Elara.TypeInfer.Infer (Infer)
import Elara.AST.Typed (TypedDeclaration)
import Elara.AST.Typed qualified as Typed 


inferDeclaration :: CanonicalDeclaration -> Infer TypedDeclaration
inferDeclaration = do
    undefined