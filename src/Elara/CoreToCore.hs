{- | Entrypoint for the Core to Core stage of compilation
This stage performs optimisations and transformations on the Core AST
-}
module Elara.CoreToCore where

import Control.Lens (transform)
import Elara.AST.Name
import Elara.AST.VarRef
import Elara.Core (CoreExpr, Expr (..), Literal (..), Var (..), mapBind)
import Elara.Core.Module (CoreDeclaration (..), CoreModule (..))

type CoreExprPass = CoreExpr -> CoreExpr

constantFold :: CoreExprPass
constantFold = transform f
  where
    f (App (App (Var (Id (Global' (Qualified "+" (ModuleName ("Prelude" :| [])))) _)) (Lit (Int a))) (Lit (Int b))) = Lit (Int ((a + b)))
    f other = other

-- | Performs beta reduction on the Core AST to reduce redundant lambdas
betaReduce :: CoreExprPass
betaReduce = transform f
  where
    f (App (Lam param body) arg) = subst param arg body
    f other = other

subst :: Var -> Expr Var -> Expr Var -> CoreExpr
subst v e = transform f
  where
    f (Var v') | v == v' = e
    f other = other

coreToCoreExpr :: CoreExprPass
coreToCoreExpr = betaReduce . constantFold

fullCoreToCoreExpr :: CoreExprPass
fullCoreToCoreExpr = fix' coreToCoreExpr
  where
    fix' f x = let x' = f x in if x == x' then x else fix' f x'

coreToCore :: CoreModule -> CoreModule
coreToCore (CoreModule name decls) = CoreModule name (fmap f decls)
  where
    f (CoreValue v) = CoreValue (mapBind identity fullCoreToCoreExpr v)
