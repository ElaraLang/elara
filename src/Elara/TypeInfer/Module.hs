module Elara.TypeInfer.Module where

import Control.Lens (Getting, preview, view)
import Control.Monad.Except (liftEither)
import Data.Map (keys, traverseMaybeWithKey)
import Data.Map qualified as Map
import Elara.AST.Canonical (CanonicalModule, Expr (LetIn, Var), LocatedExpr)

import Control.Lens.Tuple
import Data.Maybe (fromJust)
import Elara.AST.Typed (PolytypeExpr (PolytypeExpr), TypedDeclarationBody, TypedModule)
import Elara.AST.Typed qualified as Typed
import Elara.Data.Located (Located (Located), Region (Region))
import Elara.Data.Located qualified as Located
import Elara.Data.Module
import Elara.Data.Name
import Elara.Data.Name qualified as Name
import Elara.Data.Qualifications (Qualified)
import Elara.Data.Type (AbsType (..), Concrete, ConcreteType, TRec (Concrete), unconcrete)
import Elara.TypeInfer.Common (Scheme, Type ((:->)))
import Elara.TypeInfer.Common qualified as Type
import Elara.TypeInfer.Declaration
import Elara.TypeInfer.Environment (closeOver, generalize)
import Elara.TypeInfer.Error (TypeError)
import Elara.TypeInfer.Expression (inferExpr, inferExprOfType, preludeType)
import Elara.TypeInfer.Infer (Infer, InferState (typeEnv), addToEnv, maybeLookupEnv, runInfer, runSolve, toInfer, unify)
import Elara.TypeInfer.Substitute (Substitutable, apply)
import Print (debugColored)
import Prelude hiding (Type)

inferModule :: CanonicalModule -> Infer TypedModule
inferModule m@(Module name imports exposing declarations) = do
    let bodies = view body <$> declarations

    -- get all the values with their name as a map
    let values = fromMaybe Map.empty $ traverseMaybeWithKey (\_ k -> Just (preview _Value k)) bodies
    -- TODO handle the other types (TypeDef and TypeAlias)
    forM_ (keys values) addDeclarationStub
    x <- inferValues m values
    let y = Declaration name `Map.mapWithKey` Map.fromList (zip (keys values) x)

    -- declarations' <- forM (sortOn (view declarationName) (elems declarations)) inferDeclaration

    pure $ Module name imports exposing y

inferMany :: [Infer Typed.Expr] -> InferState -> Either TypeError (InferState, [(Typed.Expr, Scheme)])
inferMany [] env = pure (env, [])
inferMany m env = do
    let y = sequence m
    (s, nextEnv, cs) <- runInfer env y
    subst <- runSolve cs
    debugColored subst
    let schemes = generalize (typeEnv nextEnv) <$> apply subst (Typed.typeOf <$> s)
    pure (nextEnv, zip s schemes)

instance Substitutable (Typed.Expr) where
    apply s (Typed.Expr e t) = Typed.Expr (apply s e) (apply s t)

instance Substitutable (Typed.Expr_) where
    apply s e = Typed.transformExpr_ (apply s) e

inferValues :: CanonicalModule -> Map Name (LocatedExpr, [Void], Maybe (ConcreteType Qualified)) -> Infer [TypedDeclarationBody]
inferValues (Module modName _ _ _) vals = do
    env <- get

    forM (Map.toList vals) $ \(name, (val, _, expectedType)) -> do
        env <- get
        let inf = do
                x <- inferExpr (LetIn name val (Located (Region 0 0) (Var (QualifiedName modName name))))
                expected <- case expectedType of
                    Just t -> Just <$> inferType t
                    Nothing -> maybeLookupEnv name
                whenJust expected $ \expected' -> do
                    unify expected' (Typed.typeOf x)
                pure x

        (ty, env', cs) <- liftEither $ runInfer env inf
        subst <- liftEither $ runSolve cs

        let sc = closeOver (apply subst (Typed.typeOf ty))
        put env'
        addToEnv (name, sc)
        pure $ Value (PolytypeExpr (Located.replace ty val) sc) [] expectedType

inferType :: ConcreteType Qualified -> Infer Type
inferType (Concrete (UserDefinedType n) q) = pure (Type.UserDefinedType q n)
inferType x = inferType' (unconcrete x)
  where
    inferType' :: AbsType Concrete Qualified -> Infer Type
    inferType' (TypeVar n) = pure (Type.TypeVar (nameValue n))
    inferType' (Function a b) = liftA2 (:->) (inferType a) (inferType b)
    inferType' Unit = pure (preludeType "unit")
    inferType' (TypeConstructorApplication con arg) = liftA2 Type.TypeConstructorApplication (inferType con) (one <$> inferType arg)
    inferType' _ = error "This shouldn't happen"

previewList :: Getting (First a) s a -> s -> [a]
previewList g = maybeToList . preview g