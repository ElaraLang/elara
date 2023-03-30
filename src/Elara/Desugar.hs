{-# LANGUAGE TemplateHaskell #-}

module Elara.Desugar where

import Control.Lens
import Data.Map qualified as M
import Elara.AST.Desugared qualified as Desugared
import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Module
import Elara.AST.Name
import Elara.AST.Region
import Elara.AST.Select
import Elara.Error (ReportableError (report), writeReport)
import Elara.Error.Codes qualified as Codes
import Error.Diagnose (Report (Err))
import Polysemy
import Polysemy.Error (Error, runError, throw)
import Polysemy.MTL ()
import Polysemy.State (State, evalState)
import Unsafe.Coerce (unsafeCoerce)

newtype DesugarError
    = DefWithoutLet (Located Desugared.TypeAnnotation)

instance ReportableError DesugarError where
    report (DefWithoutLet (Located _ (Desugared.TypeAnnotation _ _))) =
        writeReport $ Err (Just Codes.defWithoutLet) "Def without let" [] []

type Desugar a = Sem [State DesugarState, Error DesugarError] a

runDesugar :: Desugar a -> Either DesugarError a
runDesugar = run . runError . evalState (DesugarState M.empty)

newtype DesugarState = DesugarState
    { _partialDeclarations :: Map Name PartialDeclaration
    }

data PartialDeclaration
    = JustDef (Located Desugared.TypeAnnotation)
    | JustLet Desugared.Expr
    | Both (Located Desugared.TypeAnnotation) Desugared.Expr
    | Immediate Desugared.DeclarationBody
    deriving (Show)

makeLenses ''DesugarState

resolvePartialDeclaration :: Located PartialDeclaration -> Desugar Desugared.DeclarationBody
resolvePartialDeclaration (Located _ (Immediate a)) = pure a
resolvePartialDeclaration l@(Located _ (JustLet e)) = pure (Desugared.DeclarationBody $ Desugared.Value e Nothing <$ l)
resolvePartialDeclaration l@(Located _ (Both ty e)) = pure (Desugared.DeclarationBody $ Desugared.Value e (Just ty) <$ l)
resolvePartialDeclaration (Located _ (JustDef ty)) = throw (DefWithoutLet ty)

desugar ::
    Module Frontend ->
    Desugar (Module Desugared)
desugar =
    traverseOf
        (_Module @Frontend @Desugared . unlocated)
        ( \(m' :: Module' Frontend) -> do
            let decls = m' ^. module'Declarations :: [Frontend.Declaration]
            decls' <- desugarDeclarations decls
            -- TODO: get rid of unsafeCoerce
            pure (Module' (m' ^. module'Name) (coerceExposing (m' ^. module'Exposing)) (coerceImport <$> m' ^. module'Imports) decls')
        )

desugarDeclarations :: [Frontend.Declaration] -> Desugar [Desugared.Declaration]
desugarDeclarations decls = do
    genPartials decls
    traverse desugarDeclaration decls

mergePartials :: PartialDeclaration -> PartialDeclaration -> Desugar PartialDeclaration
mergePartials (JustDef ty) (JustLet e) = pure (Both ty e)
mergePartials (JustLet e) (JustDef ty) = pure (Both ty e)
mergePartials _ _ = error "not possible?"

genPartials :: [Frontend.Declaration] -> Desugar ()
genPartials = traverseOf_ (each . Frontend._Declaration . unlocated) genPartial
  where
    genPartial :: Frontend.Declaration' -> Desugar ()
    genPartial (Frontend.Declaration' _ n b) =
        traverseOf_ (Frontend._DeclarationBody . unlocated) genPartial' b
      where
        genPartial' :: Frontend.DeclarationBody' -> Desugar ()
        genPartial' db = do
            partial <- genPartial'' db
            let f = insertWithM mergePartials (n ^. unlocated) partial
            modifyM (traverseOf partialDeclarations f)

        genPartial'' (Frontend.Value e pats) = do
            exp' <- desugarExpr e
            pats' <- traverse desugarPattern pats
            let body = foldLambda pats' exp'
            pure (JustLet body)
        genPartial'' (Frontend.ValueTypeDef ty) = do
            ty' <- traverse desugarTypeAnnotation ty
            pure (JustDef ty')
        genPartial'' (Frontend.TypeAlias ty) = do
            ty' <- traverse desugarType ty
            let decl = Desugared.DeclarationBody (Located (b ^. Frontend._DeclarationBody . sourceRegion) (Desugared.TypeAlias ty'))
            pure (Immediate decl)

desugarTypeAnnotation :: Frontend.TypeAnnotation -> Desugar Desugared.TypeAnnotation
desugarTypeAnnotation (Frontend.TypeAnnotation ln ty) = Desugared.TypeAnnotation ln <$> desugarType ty

desugarType :: Frontend.Type -> Desugar Desugared.Type
desugarType (Frontend.TypeVar t) = pure (Desugared.TypeVar t)
desugarType Frontend.UnitType = pure Desugared.UnitType
desugarType (Frontend.FunctionType a b) = Desugared.FunctionType <$> desugarType a <*> desugarType b
desugarType (Frontend.TypeConstructorApplication a b) = Desugared.TypeConstructorApplication <$> desugarType a <*> desugarType b
desugarType (Frontend.UserDefinedType a) = pure (Desugared.UserDefinedType a)
desugarType (Frontend.RecordType fields) = Desugared.RecordType <$> traverseOf (each . _2) desugarType fields

desugarDeclaration :: Frontend.Declaration -> Desugar Desugared.Declaration
desugarDeclaration (Frontend.Declaration ld) =
    Desugared.Declaration
        <$> traverseOf
            unlocated
            ( \d -> do
                partials <- use partialDeclarations
                let declName = d ^. Frontend.declaration'Name . unlocated
                case M.lookup declName partials of
                    Nothing -> error ("no partial declarations for" <> show declName)
                    Just partial -> do
                        body <- resolvePartialDeclaration (partial <$ ld)
                        pure (Desugared.Declaration' (d ^. Frontend.declaration'Module') (d ^. Frontend.declaration'Name) body)
            )
            ld

desugarExpr :: Frontend.Expr -> Desugar Desugared.Expr
desugarExpr (Frontend.Expr fe) = Desugared.Expr <$> traverseOf unlocated desugarExpr' fe
  where
    desugarExpr' :: Frontend.Expr' -> Desugar Desugared.Expr'
    desugarExpr' (Frontend.Int i) = pure (Desugared.Int i)
    desugarExpr' (Frontend.Float f) = pure (Desugared.Float f)
    desugarExpr' (Frontend.String s) = pure (Desugared.String s)
    desugarExpr' (Frontend.Char c) = pure (Desugared.Char c)
    desugarExpr' Frontend.Unit = pure Desugared.Unit
    desugarExpr' (Frontend.Var v) = pure (Desugared.Var v)
    desugarExpr' (Frontend.Constructor c) = pure (Desugared.Constructor c)
    desugarExpr' (Frontend.Lambda pats e) = do
        pats' <- traverse desugarPattern pats
        e' <- desugarExpr e
        pure (foldLambda pats' e' ^. Desugared._Expr . unlocated) -- Somewhat cursed but it works
    desugarExpr' (Frontend.FunctionCall e1 e2) = liftA2 Desugared.FunctionCall (desugarExpr e1) (desugarExpr e2)
    desugarExpr' (Frontend.If a b c) = liftA3 Desugared.If (desugarExpr a) (desugarExpr b) (desugarExpr c)
    desugarExpr' (Frontend.BinaryOperator o a b) = liftA3 Desugared.BinaryOperator (desugarBinaryOperator o) (desugarExpr a) (desugarExpr b)
    desugarExpr' (Frontend.List e) = Desugared.List <$> traverse desugarExpr e
    desugarExpr' (Frontend.Match e cases) = do
        e' <- desugarExpr e
        cases' <- traverse (bitraverse desugarPattern desugarExpr) cases
        pure $ Desugared.Match e' cases'
    desugarExpr' (Frontend.Let n pats body) = do
        pats' <- traverse desugarPattern pats
        body' <- desugarExpr body
        pure (Desugared.Let n (foldLambda pats' body'))
    desugarExpr' (Frontend.LetIn n pats e body) = do
        pats' <- traverse desugarPattern pats
        e' <- desugarExpr e
        body' <- desugarExpr body
        pure (Desugared.LetIn n (foldLambda pats' e') body')
    desugarExpr' (Frontend.Block e) = Desugared.Block <$> traverse desugarExpr e
    desugarExpr' (Frontend.InParens e) = Desugared.InParens <$> desugarExpr e

desugarBinaryOperator :: Frontend.BinaryOperator -> Desugar Desugared.BinaryOperator
desugarBinaryOperator (Frontend.MkBinaryOperator lop) = Desugared.MkBinaryOperator <$> traverseOf unlocated desugarBinaryOperator' lop
  where
    desugarBinaryOperator' :: Frontend.BinaryOperator' -> Desugar Desugared.BinaryOperator'
    desugarBinaryOperator' (Frontend.Op o) = pure (Desugared.Op o)
    desugarBinaryOperator' (Frontend.Infixed o) = pure (Desugared.Infixed o)

desugarPattern :: Frontend.Pattern -> Desugar Desugared.Pattern
desugarPattern (Frontend.Pattern lp) = Desugared.Pattern <$> traverseOf unlocated desugarPattern' lp
  where
    desugarPattern' :: Frontend.Pattern' -> Desugar Desugared.Pattern'
    desugarPattern' (Frontend.IntegerPattern i) = pure (Desugared.IntegerPattern i)
    desugarPattern' (Frontend.FloatPattern f) = pure (Desugared.FloatPattern f)
    desugarPattern' (Frontend.StringPattern s) = pure (Desugared.StringPattern s)
    desugarPattern' (Frontend.CharPattern c) = pure (Desugared.CharPattern c)
    desugarPattern' (Frontend.VarPattern v) = pure (Desugared.VarPattern v)
    desugarPattern' (Frontend.ConstructorPattern c pats) = Desugared.ConstructorPattern c <$> traverse desugarPattern pats
    desugarPattern' Frontend.WildcardPattern = pure Desugared.WildcardPattern
    desugarPattern' (Frontend.ListPattern pats) = Desugared.ListPattern <$> traverse desugarPattern pats

foldLambda :: [Desugared.Pattern] -> Desugared.Expr -> Desugared.Expr
foldLambda [] e = e
foldLambda (p : ps) e =
    over
        Desugared._Expr
        (\e' -> Located (e' ^. sourceRegion) (Desugared.Lambda p (foldLambda ps e)))
        e