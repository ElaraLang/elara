{-# LANGUAGE PatternSynonyms #-}

-- | Desugar the AST into Core.
module Elara.ASTToCore where

import Elara.AST.Module
import Elara.AST.Name (LowerAlphaName (..), ModuleName (..), Name (NVarName), ToName (toName), VarName (..), unqualified, _NVarName)
import Elara.AST.Select (Typed, moduleName, name)
import Elara.AST.Typed qualified as AST

import Elara.Core qualified as Core
import Elara.Core.Module qualified as Core

import Control.Lens (findOf, folded, (^.), (^?!))
import Elara.AST.Region (Located (..), unlocated)
import Elara.AST.VarRef (VarRef, VarRef' (Global, Local))
import Elara.ASTToCore.Error (ASTToCoreError (..))
import Elara.Data.TopologicalGraph (TopologicalGraph)
import Polysemy
import Polysemy.Error
import TODO (todo)

-- | Desugar the AST into Core.
desugar :: (Member (Error ASTToCoreError) r) => TopologicalGraph (Module Typed) -> Module Typed -> Sem r Core.Module
desugar path m | m ^. moduleName . unlocated == ModuleName ("Main" :| []) = do
    let mainFunction =
            findOf
                (_Module . unlocated . declarations . folded)
                (\d -> d ^. name . unlocated . unqualified == NVarName (NormalVarName (LowerAlphaName "main")))
                m
    whenNothing_ mainFunction (throw (MainModuleMissingMainFunction m))
    decls <- traverse desugarDeclaration (m ^. _Module . unlocated . declarations)
    pure (Core.MainModule (m ^. moduleName . unlocated) decls)
desugar path m = do
    decls <- traverse desugarDeclaration (m ^. _Module . unlocated . declarations)
    pure (Core.Module (m ^. moduleName . unlocated) decls)

desugarDeclaration :: AST.Declaration -> Sem r Core.Declaration
desugarDeclaration d =
    let db = d ^. AST._Declaration . unlocated
     in case db ^. AST.declaration'Body . AST._DeclarationBody . unlocated of
            AST.Value v -> do
                v' <- desugarExpr v
                let valName = (^?! _NVarName) <<$>> d ^. name
                pure (Core.Value (valName ^. unlocated) v')
            AST.TypeDeclaration _ _ _ -> todo

pattern TypedExpr :: AST.Expr' -> AST.Expr
pattern TypedExpr x <- AST.Expr (Located loc x, _)

desugarExpr :: AST.Expr -> Sem r Core.CoreExpr
desugarExpr (AST.Expr (le, ty)) = case le ^. unlocated of
    AST.Int i -> pure (Core.Lit (Core.IntLit i))
    AST.Float i -> pure (Core.Lit (Core.FloatLit i))
    AST.String i -> pure (Core.Lit (Core.StringLit i))
    AST.Char i -> pure (Core.Lit (Core.CharLit i))
    AST.Unit -> error "TODO: Unit"
    AST.Var vn -> Core.Var <$> desugarVarRef (vn ^. unlocated)
    AST.Constructor _ -> error "TODO: Constructor"
    AST.Lambda val body -> do
        let val' = Core.Local (val ^. unlocated)
        body' <- desugarExpr body
        pure (Core.Lam val' body')
    AST.FunctionCall fn arg -> do
        fn' <- desugarExpr fn
        arg' <- desugarExpr arg
        pure (Core.App fn' arg')
    AST.If cond then_ else_ -> error "TODO: If" -- We need to desugar this into a match but until I figure out types this isn't possible.
    AST.List _ -> error "TODO: List"
    AST.Match e cases -> do
        e' <- desugarExpr e
        cases' <- traverse desugarMatchCase cases
        pure (Core.Match e' cases')
    AST.LetIn name val body -> do
        let name' = Core.Local (name ^. unlocated)
        val' <- desugarExpr val
        body' <- desugarExpr body
        pure (Core.Let name' val' body')
    AST.Let name val -> error "TODO: Let"
    AST.Block _ -> error "TODO: Block"
    AST.Tuple _ -> error "TODO: Tuple"

desugarMatchCase :: (AST.Pattern, AST.Expr) -> Sem r Core.CoreCase
desugarMatchCase (p, e) = do
    todo

desugarVarRef :: VarRef VarName -> Sem r Core.VarRef
desugarVarRef (Global lqn) = pure (Core.Global $ toName <$> (lqn ^. unlocated))
desugarVarRef (Local vn) = pure (Core.Local (vn ^. unlocated))