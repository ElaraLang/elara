module Elara.Core.ToANF where

import Control.Monad.Cont
import Elara.AST.VarRef
import Elara.Core qualified as Core
import Elara.Core.ANF qualified as ANF
import Elara.Core.Analysis (exprType)
import Elara.Core.Generic (Bind (..))
import Elara.Data.Pretty
import Elara.Data.Unique
import Elara.Logging (StructuredDebug, debugWith)
import Polysemy

{- | Convert a Core expression to ANF
For example:
@let main = print (f (g (h 1)))@
becomes:
@ let main =
    let v1 = h 1 in
    let v2 = g v1 in
    let v3 = f v2 in
    print v3
@
-}

-- toANF :: Core.CoreExpr -> Sem r (ANF.Expr Core.Var)
-- toANF expr = (toANF' expr pure)

type ToANF r = (Members [UniqueGen, StructuredDebug] r, Pretty (Core.Expr Core.Var))

toANF :: ToANF r => Core.CoreExpr -> Sem r (ANF.Expr Core.Var)
toANF expr =
    debugWith ("toANF " <> pretty expr) $
        toANF' expr (\e -> pure (ANF.CExpr $ ANF.AExpr e))

toANFCont :: ToANF r => Core.CoreExpr -> ContT (ANF.Expr Core.Var) (Sem r) (ANF.AExpr Core.Var)
toANFCont e = ContT $ \k -> toANF' e k

{- | Convert a Core expression to an atomic expression in ANF,
accepting a continuation to handle the rest of the expression
-}
toANF' ::
    ToANF r =>
    Core.CoreExpr ->
    (ANF.AExpr Core.Var -> Sem r (ANF.Expr Core.Var)) ->
    Sem r (ANF.Expr Core.Var)
toANF' (Core.Lit l) k = k $ ANF.Lit l
toANF' (Core.Var v) k = k $ ANF.Var v
toANF' (Core.TyApp v t) k = toANF' v $ \v' -> k $ ANF.TyApp v' t
toANF' (Core.TyLam t e) k = toANF' e $ \e' -> k $ ANF.TyLam t e'
toANF' (Core.Lam b e) cont = evalContT $ do
    -- convert the body to ANF, making sure to not lift it out too far
    e' <- lift $ toANFRec e (pure . ANF.CExpr)
    lift $ cont $ ANF.Lam b e'
toANF' other k = debugWith ("toANF' " <> pretty other <> ": ") $ evalContT $ do
    v <- lift $ makeUnique "var"
    let id = Core.Id (Local' v) (exprType other) Nothing

    l' <- lift $ k $ ANF.Var id
    lift $ toANFRec other $ \e -> do
        pure $ ANF.Let (NonRecursive (id, e)) l'

toANFRec ::
    ToANF r =>
    Core.Expr Core.Var ->
    (ANF.CExpr Core.Var -> ContT (ANF.Expr Core.Var) (Sem r) (ANF.Expr Core.Var)) ->
    Sem r (ANF.Expr Core.Var)
toANFRec (Core.App f x) k = evalContT $ do
    f' <- toANFCont f
    x' <- toANFCont x
    k (ANF.App f' x')
toANFRec (Core.Match bind as cases) k = evalContT $ do
    bind <- toANFCont bind
    -- we need to take care here that we don't lift any expressions out too far from the match
    -- as this could lead to incorrect results as we strictly evaluate

    cases' <- for cases $ \(con, bs, e) -> do
        e' <- lift $ toANFRec e (pure . ANF.CExpr)
        pure (con, bs, e')
    k $ ANF.Match bind as cases'
toANFRec (Core.Let (NonRecursive (b, e)) body) _ = evalContT $ do
    e' <- toANFCont e
    body' <- toANFCont body
    pure $ ANF.Let (NonRecursive (b, ANF.AExpr e')) (ANF.CExpr $ ANF.AExpr body')
toANFRec other k = do
    -- DANGER of infinite loop!!! make sure all cases are covered
    toANF' other $ \e -> evalContT $ k $ ANF.AExpr e

fromANF :: ANF.Expr Core.Var -> Core.CoreExpr
fromANF (ANF.Let (NonRecursive (b, e)) body) = Core.Let (NonRecursive (b, fromANFCExpr e)) $ fromANF body
fromANF (ANF.Let (Recursive bs) body) = Core.Let (Recursive $ fromANFCExpr <<$>> bs) $ fromANF body
fromANF (ANF.CExpr e) = fromANFCExpr e

fromANFCExpr :: ANF.CExpr Core.Var -> Core.CoreExpr
fromANFCExpr (ANF.App f x) = Core.App (fromANFAtom f) (fromANFAtom x)
fromANFCExpr (ANF.Match e b alts) =
    Core.Match (fromANFAtom e) b $
        fmap (\(con, bs, e) -> (con, bs, fromANF e)) alts
fromANFCExpr (ANF.AExpr e) = fromANFAtom e

fromANFAtom :: ANF.AExpr Core.Var -> Core.Expr Core.Var
fromANFAtom (ANF.Lit i) = Core.Lit i
fromANFAtom (ANF.Var v) = Core.Var v
fromANFAtom (ANF.Lam b e) = Core.Lam b $ fromANF e
fromANFAtom (ANF.TyLam l e) = Core.TyLam l $ fromANFAtom e
fromANFAtom (ANF.TyApp e t) = Core.TyApp (fromANFAtom e) t
