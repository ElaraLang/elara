module Elara.TypeInfer.Declaration where

import Elara.AST.Canonical (CanonicalDeclaration, CanonicalDeclarationBody)
import Elara.AST.Typed (TypedDeclaration, TypedDeclarationBody)
import Elara.Data.Located qualified as Located
import Elara.Data.Module (Declaration (Declaration), DeclarationBody (TypeAlias, Value, ValueTypeDef))
import Elara.TypeInfer.Expression (inferExpr)
import Elara.TypeInfer.Infer (Infer)

inferDeclaration :: CanonicalDeclaration -> Infer TypedDeclaration
inferDeclaration (Declaration m name body) = do
    body' <- inferDeclarationBody body
    pure $ Declaration m name body'

inferDeclarationBody :: CanonicalDeclarationBody -> Infer TypedDeclarationBody
inferDeclarationBody (ValueTypeDef a) = pure $ ValueTypeDef a
inferDeclarationBody (TypeAlias a) = pure $ TypeAlias a
inferDeclarationBody (Value e p a) = do
    e' <- inferExpr (Located.unlocate e)
    pure $ Value (Located.replace e' e) p a
