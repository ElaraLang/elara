module Elara.TypeInfer.Expression where
import           Control.Monad.Error.Class      ( liftEither )
import           Control.Monad.RWS              ( listen )
import           Data.List.NonEmpty            as NE
                                                ( zipWith )
import           Data.Maybe                     ( fromJust )
import qualified Elara.AST.Canonical           as Can
import qualified Elara.AST.Typed               as Typed
import           Elara.Data.Located
import qualified Elara.Data.Located            as Located
import           Elara.Data.Name as Name
import           Elara.TypeInfer.Common
import           Elara.TypeInfer.Environment
import           Elara.TypeInfer.Infer
import           Elara.TypeInfer.Substitute
import           Prelude                 hiding ( lookupEnv )
import           Print

preludeType :: Name -> Typed.Type
preludeType = Typed.UserDefinedType (ModuleName ("Prelude" :| []))




inferExpr :: Can.Expr -> Infer Typed.Expr
inferExpr = \case
  Can.Int      x -> pure $ Typed.Expr (Typed.Int x) (preludeType "Int")
  Can.Float    x -> pure $ Typed.Expr (Typed.Float x) (preludeType "Float")
  Can.Char     x -> pure $ Typed.Expr (Typed.Char x) (preludeType "Char")
  Can.String   x -> pure $ Typed.Expr (Typed.String x) (preludeType "String")
  Can.Bool     x -> pure $ Typed.Expr (Typed.Bool x) (preludeType "Bool")
  Can.Argument x -> do
    type' <- lookupEnv x
    pure $ Typed.Expr (Typed.Argument x) type'
  Can.Block exprs -> do
    xs <- mapM inferExpr (unlocate <$> exprs)
    let locations = getRegion <$> exprs
    pure $ Typed.Expr (Typed.Block (NE.zipWith Located locations xs))
                      (Typed.typeOf (last xs))

  Can.LetIn name val body -> do
    env                     <- gets typeEnv
    (typedVal, constraints) <- listen $ inferExpr (unlocate val)
    subst                   <- liftEither $ runSolve constraints
    let sc = generalize (apply subst env) (apply subst (Typed.typeOf typedVal))

    typedBody <- inEnv (name, sc) $ inferExpr (unlocate body)
    expected  <- maybeLookupEnv name
    when (isJust expected) $ do
      let actual = fromJust expected
      unify (Typed.typeOf typedBody) actual
    let locatedVal  = Located.replace typedVal val
    let locatedBody = Located.replace typedBody body
    pure $ Typed.Expr (Typed.LetIn name locatedVal locatedBody)
                      (Typed.typeOf typedBody)

  Can.Lambda arg body -> do
    argType <- freshTypeVariable
    t       <- inEnv (show arg, Forall [] argType) (inferExpr (unlocate body))
    let typedBody = Located.replace t body
    typedArg <- inferPattern arg
    pure $ Typed.Expr (Typed.Lambda typedArg typedBody)
                      (argType Typed.:-> Typed.typeOf t)
  other -> error $ "Not implemented: " <> prettyShow other


inferPattern :: Can.Pattern -> Infer Typed.Pattern
inferPattern = \case
  Can.NamedPattern name -> do
    pure $ Typed.Pattern (Typed.NamedPattern name) Nothing
  other -> error $ "Not implemented: " <> prettyShow other
