{-# LANGUAGE PatternSynonyms #-}

{- | Entrypoint for the Core to Core stage of compilation
This stage performs optimisations and transformations on the Core AST
-}
module Elara.CoreToCore where

import Effectful (Eff)
import Elara.AST.Name
import Elara.AST.VarRef
import Elara.Core (CoreBind, CoreExpr, Expr (..), Literal (..), Var (..))
import Elara.Core qualified as Core
import Elara.Core.ANF qualified as ANF
import Elara.Core.Generic (Bind (..), mapBind, traverseBind)
import Elara.Core.Module (CoreDeclaration (..), CoreModule (..))
import Elara.Core.ToANF
import Elara.Core.TypeCheck (TypeCheckError, typeCheckCoreModule)
import Elara.Error (runErrorOrReport)
import Elara.Query qualified
import Elara.Query.Effects (ConsQueryEffects)
import Polysemy hiding (transform)
import Rock qualified

type CoreExprPass = CoreExpr -> CoreExpr

pattern Infix :: NonEmpty Text -> Text -> CoreExpr -> CoreExpr -> CoreExpr
pattern Infix mn op a b <-
    App
        ( App
                (Var (Id (Global (Qualified op (ModuleName mn))) _ _))
                a
            )
        b

constantFold :: CoreExprPass
constantFold = transform f
  where
    f (Infix ("Prelude" :| []) "+" (Lit (Int a)) (Lit (Int b))) = Lit (Int (a + b))
    f other = other

-- | Performs beta reduction on the Core AST to reduce redundant lambdas
betaReduce :: CoreExprPass
betaReduce = transform f
  where
    f (App (Lam param body) arg) = subst param arg body
    f other = other

uselessLetInline :: CoreExprPass
uselessLetInline = transform f
  where
    f (Let (NonRecursive (b, e)) body) | body == Var b = e
    f other = other

pipeInline :: CoreExprPass
pipeInline = transform f
  where
    f (Infix ("Elara" :| ["Prim"]) "|>" a b) = App b a
    f other = other

subst :: Var -> Expr Var -> Expr Var -> CoreExpr
subst v e = transform f
  where
    f (Var v') | v == v' = e
    f other = other

coreToCoreExpr :: CoreExprPass
coreToCoreExpr =
    betaReduce . constantFold . pipeInline . uselessLetInline

fullCoreToCoreExpr :: CoreExprPass
fullCoreToCoreExpr = fix' coreToCoreExpr
  where
    fix' f x = let x' = f x in if x == x' then x else fix' f x'

coreToCore :: CoreModule CoreBind -> CoreModule CoreBind
coreToCore (CoreModule name decls) = CoreModule name (fmap f decls)
  where
    f (CoreValue v) = CoreValue (mapBind identity fullCoreToCoreExpr v)
    f other = other

runGetOptimisedCoreModuleQuery :: ModuleName -> Eff (ConsQueryEffects '[Rock.Rock Elara.Query.Query]) (CoreModule CoreBind)
runGetOptimisedCoreModuleQuery mn = do
    coreModule <- Rock.fetch (Elara.Query.GetCoreModule mn)
    pure $ coreToCore coreModule

runGetANFCoreModuleQuery ::
    ModuleName ->
    Eff
        (ConsQueryEffects '[Rock.Rock Elara.Query.Query])
        (CoreModule (ANF.TopLevelBind Core.Var))
runGetANFCoreModuleQuery mn = do
    coreModule <- Rock.fetch (Elara.Query.GetOptimisedCoreModule mn)
    moduleToANF coreModule

runGetFinalisedCoreModuleQuery :: ModuleName -> Eff (ConsQueryEffects '[Rock.Rock Elara.Query.Query]) (CoreModule CoreBind)
runGetFinalisedCoreModuleQuery mn = do
    coreModule <- Rock.fetch (Elara.Query.GetClosureLiftedModule mn)
    runErrorOrReport @TypeCheckError $ typeCheckCoreModule coreModule
    pure (unANF coreModule)

moduleToANF ::
    ToANF r =>
    CoreModule (Elara.Core.Generic.Bind Var Core.Expr) ->
    Eff r (CoreModule (ANF.TopLevelBind Var))
moduleToANF (CoreModule name decls) = CoreModule name <$> traverse f decls
  where
    f (CoreValue v) = CoreValue <$> traverseBind pure toANF v
    f (CoreType t) = pure (CoreType t)

unANF ::
    CoreModule (Elara.Core.Generic.Bind Var ANF.Expr) ->
    CoreModule (Elara.Core.Generic.Bind Var Expr)
unANF (CoreModule name decls) = CoreModule name (fmap f decls)
  where
    f (CoreValue v) = CoreValue (mapBind identity fromANF v)
    f (CoreType t) = CoreType t
