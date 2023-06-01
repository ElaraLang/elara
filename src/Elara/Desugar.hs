{-# LANGUAGE TemplateHaskell #-}

module Elara.Desugar where

import Control.Lens
import Data.Map qualified as M
import Elara.AST.Desugared qualified as Desugared
import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Module
import Elara.AST.Name hiding (name)
import Elara.AST.Region
import Elara.AST.Select
import Elara.Error (ReportableError (report), writeReport)
import Elara.Error.Codes qualified as Codes
import Error.Diagnose (Report (Err))
import Polysemy
import Polysemy.Error (Error, runError, throw)
import Polysemy.State (State, evalState)
import Polysemy.State.Extra

newtype DesugarError
    = DefWithoutLet (Located Desugared.Type)

instance ReportableError DesugarError where
    report (DefWithoutLet (Located _ _)) =
        writeReport $ Err (Just Codes.defWithoutLet) "Def without let" [] []

type Desugar a = Sem [State DesugarState, Error DesugarError] a

runDesugar :: Desugar a -> Either DesugarError a
runDesugar = run . runError . evalState (DesugarState M.empty)

newtype DesugarState = DesugarState
    { _partialDeclarations :: Map (IgnoreLocation Name) PartialDeclaration
    }

{- | A partial declaration stores a desugared part of a declaration
This allows merging of declarations with the same name
For example, the code
@
def a : Int
...
...
let a = 5
@
is legal, and the 2 parts of the declaration need to be merged

Firstly, we create a 'JustDef' after seeing the @def@ line, then we merge this with a 'JustLet' after seeing the @let@ line
to create a 'Both' declaration, which is then resolved to a 'Desugared.Declaration'
-}
data PartialDeclaration
    = -- | A partial declaration with just a def line
      JustDef
        SourceRegion
        -- ^ The *overall* region of the declaration, not just the body!
        (Located Desugared.Type)
    | JustLet SourceRegion Desugared.Expr
    | Both SourceRegion (Located Desugared.Type) Desugared.Expr
    | Immediate Desugared.DeclarationBody
    deriving (Show)

makeLenses ''DesugarState

resolvePartialDeclaration :: PartialDeclaration -> Desugar Desugared.DeclarationBody
resolvePartialDeclaration (Immediate a) = pure a
resolvePartialDeclaration ((JustDef _ ty)) = throw (DefWithoutLet ty)
resolvePartialDeclaration ((JustLet sr e)) = pure (Desugared.DeclarationBody (Located sr (Desugared.Value e Nothing)))
resolvePartialDeclaration ((Both sr ty e)) = pure (Desugared.DeclarationBody (Located sr (Desugared.Value e (Just ty))))

desugar ::
    Module Frontend ->
    Desugar (Module Desugared)
desugar =
    traverseOf
        (_Module . unlocated)
        ( \(m' :: Module' Frontend) -> do
            let decls = m' ^. module'Declarations :: [Frontend.Declaration]
            decls' <- desugarDeclarations (m' ^. name) decls
            pure (Module' (m' ^. module'Name) (coerceExposing (m' ^. module'Exposing)) (coerceImport <$> m' ^. module'Imports) decls')
        )

desugarDeclarations :: Located ModuleName -> [Frontend.Declaration] -> Desugar [Desugared.Declaration]
desugarDeclarations mn decls = do
    genPartials decls
    completePartials mn

mergePartials :: PartialDeclaration -> PartialDeclaration -> Desugar PartialDeclaration
mergePartials (JustDef sr ty) (JustLet sr' e) = pure (Both (sr <> sr') ty e)
mergePartials (JustLet sr e) (JustDef sr' ty) = pure (Both (sr <> sr') ty e)
mergePartials _ _ = error "not possible?"

genPartials :: [Frontend.Declaration] -> Desugar ()
genPartials = traverseOf_ (each . Frontend._Declaration) genPartial
  where
    genPartial :: Located Frontend.Declaration' -> Desugar ()
    genPartial (Located wholeDeclRegion (Frontend.Declaration' _ n b)) =
        traverseOf_ (Frontend._DeclarationBody . unlocated) genPartial' b
      where
        genPartial' :: Frontend.DeclarationBody' -> Desugar ()
        genPartial' db = do
            partial <- genPartial'' db
            let f = insertWithM mergePartials (IgnoreLocation n) partial
            modifyM (traverseOf partialDeclarations f)

        genPartial'' :: Frontend.DeclarationBody' -> Desugar PartialDeclaration
        genPartial'' (Frontend.Value e pats) = do
            exp' <- desugarExpr e
            pats' <- traverse desugarPattern pats
            let body = foldLambda pats' exp'
            pure (JustLet wholeDeclRegion body)
        genPartial'' (Frontend.ValueTypeDef ty) = do
            ty' <- traverse desugarType ty
            pure (JustDef wholeDeclRegion ty')
        genPartial'' (Frontend.TypeDeclaration vars typeDecl) = do
            let traverseDecl :: Frontend.TypeDeclaration -> Desugar Desugared.TypeDeclaration
                traverseDecl (Frontend.Alias t) = Desugared.Alias <$> traverseOf unlocated desugarType t
                traverseDecl (Frontend.ADT constructors) = Desugared.ADT <$> traverseOf (each . _2 . each . unlocated) desugarType constructors
            typeDecl' <- traverseOf unlocated traverseDecl typeDecl
            let decl' = Desugared.TypeDeclaration vars typeDecl'
            let bodyLoc = b ^. Frontend._DeclarationBody . sourceRegion
            pure (Immediate (Desugared.DeclarationBody (Located bodyLoc decl')))

desugarType :: Frontend.Type -> Desugar Desugared.Type
desugarType (Frontend.TypeVar t) = pure (Desugared.TypeVar t)
desugarType Frontend.UnitType = pure Desugared.UnitType
desugarType (Frontend.FunctionType a b) = Desugared.FunctionType <$> traverseOf unlocated desugarType a <*> traverseOf unlocated desugarType b
desugarType (Frontend.TypeConstructorApplication a b) = Desugared.TypeConstructorApplication <$> traverseOf unlocated desugarType a <*> traverseOf unlocated desugarType b
desugarType (Frontend.UserDefinedType a) = pure (Desugared.UserDefinedType a)
desugarType (Frontend.RecordType fields) = Desugared.RecordType <$> traverseOf (each . _2 . unlocated) desugarType fields
desugarType (Frontend.TupleType fields) = Desugared.TupleType <$> traverse (traverseOf unlocated desugarType) fields

completePartials :: Located ModuleName -> Desugar [Desugared.Declaration]
completePartials mn = do
    partials <- use' partialDeclarations
    decls <-
        M.traverseWithKey
            ( \declName partial -> do
                body <- resolvePartialDeclaration partial
                let locatedName = declName ^. _IgnoreLocation
                let declaration' = Desugared.Declaration' mn locatedName body
                let overallLocation = locatedName ^. sourceRegion <> body ^. Desugared._DeclarationBody . sourceRegion
                pure (Desugared.Declaration (Located overallLocation declaration'))
            )
            partials
    pure (M.elems decls)

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
    desugarExpr' (Frontend.Tuple e) = Desugared.Tuple <$> traverse desugarExpr e

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

{-

\| Lambdas need quite a lot of desugaring - they need to be unfolded into a series of nested lambdas, and then each pattern needs to be converted into a match expression.

For example, @\a (b, c) 1 -> e@ becomes
@\a -> \b_1 -> \c_1 -> match b_1 with
                            (b, c) -> match 1 with
                                        1 -> e@

However, converting the matches would require renaming, and we're not able to do that yet.
Instead, we unfold the lambda, but keep the patterns, and the renamer handles the match conversion.

-}

foldLambda :: [Desugared.Pattern] -> Desugared.Expr -> Desugared.Expr
foldLambda [] e = e
foldLambda (p : ps) e =
    over
        Desugared._Expr
        (\e' -> Located (e' ^. sourceRegion) (Desugared.Lambda p (foldLambda ps e)))
        e
