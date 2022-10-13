module Elara.TypeInfer.Declaration where

import Control.Monad.Except (liftEither)
import Data.Maybe (fromJust)
import Elara.AST.Canonical (CanonicalDeclaration, CanonicalDeclarationBody)
import Elara.AST.Typed (PolytypeExpr (PolytypeExpr), TypedDeclaration, TypedDeclarationBody)
import Elara.AST.Typed qualified as Typed
import Elara.Data.Located qualified as Located
import Elara.Data.Module (Declaration (Declaration), DeclarationBody (TypeAlias, Value, ValueTypeDef))
import Elara.TypeInfer.Common (Scheme (Forall), Type (TypeVar), TypeVariable (..))
import Elara.TypeInfer.Environment (extend, normalize)
import Elara.TypeInfer.Expression (inferExpr)
import Elara.TypeInfer.Infer (Infer (..), InferState (typeEnv), freshTypeVariable, infer, inferScheme, maybeLookupEnv, unify)
import Prelude hiding (Type)

inferDeclaration :: CanonicalDeclaration -> Infer TypedDeclaration
inferDeclaration (Declaration m name body) = do
    existingBinding <- maybeLookupEnv name
    body' <- inferDeclarationBody existingBinding body
    pure $ Declaration m name body'

addDeclarationStub :: CanonicalDeclaration -> Infer ()
addDeclarationStub (Declaration _ name _) = do
    f <- freshTypeVariable
    let scheme = Forall [] f
    env <- gets typeEnv
    let newEnv = extend env (name, scheme)
    modify (\s -> s{typeEnv = newEnv})

inferDeclarationBody :: Maybe Type -> CanonicalDeclarationBody -> Infer TypedDeclarationBody
inferDeclarationBody _ (ValueTypeDef a) = pure $ ValueTypeDef a
inferDeclarationBody _ (TypeAlias a) = pure $ TypeAlias a
inferDeclarationBody existing (Value e p a) = do
    env <- get
    (x, newEnv, scheme) <- liftEither $ infer Typed.typeOf (inferExpr (Located.unlocate e)) env $ \t -> forM_ existing (unify t)
    modify (\s -> s{typeEnv = newEnv})
    pure $ Value (PolytypeExpr (Located.replace x e) scheme) p a
