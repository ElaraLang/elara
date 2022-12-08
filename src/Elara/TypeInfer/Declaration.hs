module Elara.TypeInfer.Declaration where

import Control.Monad.Except (liftEither)
import Control.Monad.ListM (foldM1)
import Elara.AST.Canonical (CanonicalDeclaration, CanonicalDeclarationBody, Expr (body))
import Elara.AST.Typed (PolytypeExpr (PolytypeExpr), TypedDeclaration, TypedDeclarationBody)
import Elara.AST.Typed qualified as Typed
import Elara.Data.Located qualified as Located
import Elara.Data.Module (Declaration (Declaration), DeclarationBody (TypeAlias, Value, ValueTypeDef))
import Elara.Data.Name
import Elara.TypeInfer.Common (Scheme (Forall), TypeVariable (TV))
import Elara.TypeInfer.Environment (closeOver, generalize)
import Elara.TypeInfer.Expression (inferExpr)
import Elara.TypeInfer.Infer (Infer (..), InferState (typeEnv), addToEnv, freshTypeVariable, inferScheme, maybeLookupEnv, runInfer, runSolve, unify)
import Elara.TypeInfer.Substitute (apply)
import Print (debugColored)
import Prelude hiding (Type)

-- inferDeclaration :: CanonicalDeclaration -> Infer TypedDeclaration
-- inferDeclaration (Declaration m name body) = do
--     body' <- inferDeclarationBody name body
--     pure $ Declaration m name body'

-- inferMany :: [CanonicalDeclaration] -> Infer [TypedDeclaration]
-- inferMany decls = do
--     env <- get
--     let (typeDefs, typeAliases, values) = undefined
--     undefined

addDeclarationStub :: Name -> Infer ()
addDeclarationStub name = do
    f <- freshTypeVariable
    let scheme = Forall [] f
    addToEnv (name, scheme)

-- -- inferDeclarationBody :: Name -> CanonicalDeclarationBody -> Infer TypedDeclarationBody
-- -- inferDeclarationBody _ (ValueTypeDef a) = pure $ ValueTypeDef a -- TODO: add the type to the environment
-- -- inferDeclarationBody _ (TypeAlias a) = pure $ TypeAlias a
-- -- inferDeclarationBody name (Value e p a) = do
-- --     env <- get
-- --     (ty, env', cs) <- liftEither $ runInfer env (inferExpr (Located.unlocate e))
-- --     subst <- liftEither $ runSolve cs
-- --     let sc = closeOver (apply subst (Typed.typeOf ty))
-- --     modify (\s -> s{typeEnv = env'})
-- --     existing <- maybeLookupEnv name
-- --     whenJust existing $ \t -> do
-- --         debugColored (Typed.typeOf ty, t)

-- --     addToEnv (name, sc)
-- --     pure $ Value (PolytypeExpr (Located.replace ty e) sc) p a