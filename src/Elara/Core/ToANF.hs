module Elara.Core.ToANF where

import Control.Monad.Cont
import Elara.AST.VarRef
import Elara.Core qualified as Core
import Elara.Core.ANF qualified as ANF
import Elara.Core.Analysis (exprType)
import Elara.Data.Unique
import Polysemy

toANF :: Member UniqueGen r => Core.CoreExpr -> Sem r (ANF.Expr Core.Var)
toANF (Core.Lit i) = pure $ ANF.AExpr $ ANF.Lit i
toANF (Core.Var v) = pure $ ANF.AExpr $ ANF.Var v
toANF (Core.App f x) = evalContT $ do
    f' <- toANFAtom' f
    x' <- toANFAtom' x
    pure $ ANF.CExpr $ ANF.App f' x'
toANF (Core.Lam b e) = ANF.AExpr . ANF.Lam b <$> toANF e
toANF (Core.TyLam l e) = evalContT $ do
    e' <- toANFAtom' e
    pure $ ANF.AExpr $ ANF.TyLam l e'
toANF (Core.Let (Core.NonRecursive (b, e)) body) = do
    e' <- toANF e
    body' <- toANF body
    pure $ ANF.Let (ANF.NonRecursive (b, e')) body'
toANF (Core.Let (Core.Recursive bs) body) = do
    bs' <- traverse (\(b, e) -> (b,) <$> toANF e) bs
    body' <- toANF body
    pure $ ANF.Let (ANF.Recursive bs') body'
toANF (Core.Match e b alts) = evalContT $ do
    e' <- toANFAtom' e
    alts' <- traverse (\(con, bs, e) -> (con,bs,) <$> lift (toANF e)) alts
    pure $ ANF.CExpr $ ANF.Match e' b alts'
toANF (Core.TyApp e t) = evalContT $ do
    e' <- toANFAtom' e
    pure $ ANF.AExpr $ ANF.TyApp e' t

toANFAtom' ::
    Member UniqueGen r =>
    Core.Expr Core.Var ->
    ContT (ANF.Expr Core.Var) (Sem r) (ANF.AExpr Core.Var)
toANFAtom' v = ContT $ \k -> toANFAtom v k

toANFAtom ::
    Member UniqueGen r =>
    Core.Expr Core.Var ->
    (ANF.AExpr Core.Var -> Sem r (ANF.Expr Core.Var)) ->
    Sem r (ANF.Expr Core.Var)
toANFAtom (Core.Lit i) k = k $ ANF.Lit i
toANFAtom (Core.Var v) k = k $ ANF.Var v
toANFAtom e k = do
    v <- makeUnique "var"
    let var = Core.Id (Local' v) (exprType e) Nothing
    e' <- toANF e
    b <- k $ ANF.Var var
    pure $ ANF.Let (ANF.NonRecursive (var, e')) b

fromANF :: ANF.Expr Core.Var -> Core.CoreExpr
fromANF (ANF.AExpr e) = fromANFAtom e
fromANF (ANF.Let (ANF.NonRecursive (b, e)) body) = Core.Let (Core.NonRecursive (b, fromANF e)) $ fromANF body
fromANF (ANF.Let (ANF.Recursive bs) body) = Core.Let (Core.Recursive $ fromANF <<$>> bs) $ fromANF body
fromANF (ANF.CExpr (ANF.App f x)) = Core.App (fromANFAtom f) (fromANFAtom x)
fromANF (ANF.CExpr (ANF.Match e b alts)) = Core.Match (fromANFAtom e) b $ fmap (\(con, bs, e) -> (con, bs, fromANF e)) alts

fromANFAtom :: ANF.AExpr Core.Var -> Core.Expr Core.Var
fromANFAtom (ANF.Lit i) = Core.Lit i
fromANFAtom (ANF.Var v) = Core.Var v
fromANFAtom (ANF.Lam b e) = Core.Lam b $ fromANF e
fromANFAtom (ANF.TyLam l e) = Core.TyLam l $ fromANFAtom e
fromANFAtom (ANF.TyApp e t) = Core.TyApp (fromANFAtom e) t
