module Elara.Core.ToANF where

import Control.Monad.Cont
import Elara.AST.VarRef
import Elara.Core qualified as Core
import Elara.Core.ANF qualified as ANF
import Elara.Core.Analysis (exprType)
import Elara.Data.Unique
import Polysemy

toANF :: Member UniqueGen r => Core.CoreExpr -> Sem r (ANF.Expr Core.Var)
toANF (Core.App f x) = evalContT $ do
    f' <- toANFAtom' f
    x' <- toANFAtom' x
    pure $ ANF.CExpr $ ANF.App f' x'
toANF (Core.Let (Core.NonRecursive (b, e)) body) = evalContT $ do
    e' <- toANFAtom' e
    body' <- lift $ toANF body
    pure $ ANF.Let (ANF.NonRecursive (b, ANF.AExpr e')) body'
toANF (Core.Let (Core.Recursive bs) body) = evalContT $ do
    bs' <- traverse (\(b, e) -> (b,) <$> (ANF.AExpr <$> toANFAtom' e)) bs
    body' <- lift $ toANF body
    pure $ ANF.Let (ANF.Recursive bs') body'
toANF (Core.Match e b alts) = evalContT $ do
    e' <- toANFAtom' e
    alts' <- traverse (\(con, bs, e) -> (con,bs,) <$> lift (toANF e)) alts
    pure $ ANF.CExpr $ ANF.Match e' b alts'
toANF other = evalContT $ do
    ANF.CExpr . ANF.AExpr <$> toANFAtom' other

toANFAtom' ::
    Member UniqueGen r =>
    Core.Expr Core.Var ->
    ContT (ANF.Expr Core.Var) (Sem r) (ANF.AExpr Core.Var)
toANFAtom' v = ContT $ \k -> toANFAtom v k

{- | Convert a Core expression to ANF
Takes a continuation to handle the result of the conversion
-}
toANFAtom ::
    Member UniqueGen r =>
    Core.Expr Core.Var ->
    (ANF.AExpr Core.Var -> Sem r _) ->
    Sem r _
toANFAtom (Core.Lit i) k = k $ ANF.Lit i
toANFAtom (Core.Var v) k = k $ ANF.Var v
toANFAtom (Core.Lam b e) k = do
    e' <- toANF e
    k $ ANF.Lam b e'
toANFAtom (Core.TyLam l e) k = evalContT $ do
    e' <- toANFAtom' e
    lift $ k $ ANF.TyLam l e'
toANFAtom (Core.TyApp e t) k = evalContT $ do
    e' <- toANFAtom' e
    lift $ k $ ANF.TyApp e' t
toANFAtom e k = do
    v <- makeUnique "var"
    let var = Core.Id (Local' v) (exprType e) Nothing
    e' <- toANF e
    case e' of
        ANF.CExpr e -> do
            b <- k $ ANF.Var var
            pure $ ANF.Let (ANF.NonRecursive (var, e)) b
        ANF.Let bind val -> do
            toANFAtom (fromANF val) $ \val' -> do
                b <- k $ ANF.Var var
                pure $ ANF.Let (ANF.NonRecursive (var, ANF.AExpr val')) b

fromANF :: ANF.Expr Core.Var -> Core.CoreExpr
fromANF (ANF.Let (ANF.NonRecursive (b, e)) body) = Core.Let (Core.NonRecursive (b, fromANFCExpr e)) $ fromANF body
fromANF (ANF.Let (ANF.Recursive bs) body) = Core.Let (Core.Recursive $ fromANFCExpr <<$>> bs) $ fromANF body
fromANF (ANF.CExpr e) = fromANFCExpr e

fromANFCExpr :: ANF.CExpr Core.Var -> Core.CoreExpr
fromANFCExpr (ANF.App f x) = Core.App (fromANFAtom f) (fromANFAtom x)
fromANFCExpr (ANF.Match e b alts) = Core.Match (fromANFAtom e) b $ fmap (\(con, bs, e) -> (con, bs, fromANF e)) alts
fromANFCExpr (ANF.AExpr e) = fromANFAtom e

fromANFAtom :: ANF.AExpr Core.Var -> Core.Expr Core.Var
fromANFAtom (ANF.Lit i) = Core.Lit i
fromANFAtom (ANF.Var v) = Core.Var v
fromANFAtom (ANF.Lam b e) = Core.Lam b $ fromANF e
fromANFAtom (ANF.TyLam l e) = Core.TyLam l $ fromANFAtom e
fromANFAtom (ANF.TyApp e t) = Core.TyApp (fromANFAtom e) t
