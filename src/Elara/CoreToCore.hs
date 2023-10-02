{- | Entrypoint for the Core to Core stage of compilation
This stage performs optimisations and transformations on the Core AST
-}
module Elara.CoreToCore where

import Control.Lens (transform)
import Elara.Core (CoreExpr, Expr (..), Var (..), mapBind)
import Elara.Core.Module (CoreDeclaration (..), CoreModule (..))
import Print (showPretty)

betaReduce :: CoreExpr -> CoreExpr
betaReduce = transform f
  where
    f x@(App (Lam param body) arg) = error (showPretty x) $ subst param arg body
    f other = other

subst :: Var -> Expr Var -> Expr Var -> CoreExpr
subst v e = transform f
  where
    f (Var v') | v == v' = e
    f other = other

coreToCoreExpr :: CoreExpr -> CoreExpr
coreToCoreExpr = betaReduce

coreToCore :: CoreModule -> CoreModule
coreToCore (CoreModule name decls) = CoreModule name (fmap f decls)
  where
    f (CoreValue v) = CoreValue (mapBind identity coreToCoreExpr v)
