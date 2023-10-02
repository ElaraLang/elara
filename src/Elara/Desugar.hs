{-# LANGUAGE TemplateHaskell #-}

module Elara.Desugar where

import Control.Lens (Each (each), makeLenses, over, traverseOf, traverseOf_, (^.), _1, _2)
import Data.Generics.Product (field', the)
import Data.Generics.Wrapped
import Data.Map qualified as M
import Elara.AST.Desugared
import Elara.AST.Frontend
import Elara.AST.Generic
import Elara.AST.Generic.Common
import Elara.AST.Module
import Elara.AST.Name hiding (name)
import Elara.AST.Region
import Elara.AST.Select
import Elara.Data.Pretty (Pretty (pretty), (<+>))
import Elara.Error (ReportableError (report), runErrorOrReport, writeReport)
import Elara.Error.Codes qualified as Codes
import Elara.Pipeline
import Elara.Utils (curry3)
import Error.Diagnose (Marker (..), Note (..), Report (Err))
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.State (State, evalState)
import Polysemy.State.Extra
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (Op)

data DesugarError
    = DefWithoutLet DesugaredType
    | DuplicateDeclaration PartialDeclaration PartialDeclaration
    | PartialNamesNotEqual PartialDeclaration PartialDeclaration
    deriving (Typeable, Show)

instance Exception DesugarError

instance ReportableError DesugarError where
    report (DefWithoutLet _) =
        writeReport $ Err (Just Codes.defWithoutLet) "Def without let" [] []
    report (DuplicateDeclaration a b) =
        writeReport $
            Err
                (Just Codes.duplicateDefinition)
                ("Duplicate declaration names:" <+> pretty a)
                [ (sourceRegionToDiagnosePosition $ partialDeclarationSourceRegion b, This "Name is used here")
                , (sourceRegionToDiagnosePosition $ partialDeclarationSourceRegion a, This "And also here")
                ]
                [ Note "Having multiple variables with the same name makes it impossible to tell which one you want to use!"
                , Hint "Rename one of the declarations"
                ]
    report (PartialNamesNotEqual a b) =
        writeReport $ Err (Just Codes.partialNamesNotEqual) ("Partial names not equal: " <+> pretty a <+> "and" <+> pretty b) [] []

type Desugar a = Sem DesugarPipelineEffects a

type DesugarPipelineEffects = '[State DesugarState, Error DesugarError]

runDesugar :: Desugar a -> Sem (EffectsAsPrefixOf DesugarPipelineEffects r) a
runDesugar = subsume_

runDesugarPipeline :: (IsPipeline r) => Sem (EffectsAsPrefixOf DesugarPipelineEffects r) a -> Sem r a
runDesugarPipeline =
    runErrorOrReport @DesugarError
        . evalState (DesugarState M.empty)

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
        Name
        -- ^ Name of the declaration
        SourceRegion
        -- ^ The *overall* region of the declaration, not just the body!
        DesugaredType
    | JustLet
        Name
        SourceRegion
        DesugaredExpr
    | Both Name SourceRegion DesugaredType DesugaredExpr
    | Immediate Name DesugaredDeclarationBody
    deriving (Typeable, Show)

partialDeclarationSourceRegion :: PartialDeclaration -> SourceRegion
partialDeclarationSourceRegion (JustDef _ sr _) = sr
partialDeclarationSourceRegion (JustLet _ sr _) = sr
partialDeclarationSourceRegion (Both _ sr _ _) = sr
partialDeclarationSourceRegion (Immediate _ (DeclarationBody (Located sr _))) = sr

instance Pretty PartialDeclaration where
    pretty (JustDef n _ _) = pretty n
    pretty (JustLet n _ _) = pretty n
    pretty (Both n _ _ _) = pretty n
    pretty (Immediate n _) = pretty n

makeLenses ''DesugarState

resolvePartialDeclaration :: PartialDeclaration -> Desugar DesugaredDeclarationBody
resolvePartialDeclaration (Immediate _ a) = pure a
resolvePartialDeclaration ((JustDef _ _ ty)) = throw (DefWithoutLet ty)
resolvePartialDeclaration ((JustLet _ sr e)) = pure (DeclarationBody (Located sr (Value e NoFieldValue Nothing)))
resolvePartialDeclaration ((Both _ sr ty e)) =
    pure
        ( DeclarationBody
            (Located sr (Value e NoFieldValue (Just ty)))
        )

desugar ::
    Module 'Frontend ->
    Desugar (Module 'Desugared)
desugar =
    traverseOf
        (_Unwrapped . unlocated)
        ( \(m' :: Module' 'Frontend) -> do
            let decls = m' ^. field' @"declarations" :: [FrontendDeclaration]
            decls' <- desugarDeclarations (m' ^. the @(Located ModuleName)) decls

            pure (Module' (m' ^. field' @"name") (coerceExposing (m' ^. field' @"exposing")) (coerceImport <$> m' ^. field' @"imports") decls')
        )

desugarDeclarations :: Located ModuleName -> [FrontendDeclaration] -> Desugar [DesugaredDeclaration]
desugarDeclarations mn decls = do
    genPartials decls
    completePartials mn

assertPartialNamesEqual :: (PartialDeclaration, Name) -> (PartialDeclaration, Name) -> Desugar Name
assertPartialNamesEqual (p1, n1) (p2, n2) = if n1 == n2 then pure n2 else throw (PartialNamesNotEqual p1 p2)

mergePartials :: PartialDeclaration -> PartialDeclaration -> Desugar PartialDeclaration
mergePartials p1@(JustDef n sr ty) p2@(JustLet n' sr' e) = do
    n'' <- assertPartialNamesEqual (p1, n) (p2, n')
    pure (Both n'' (sr <> sr') ty e)
mergePartials p1@(JustLet n sr e) p2@(JustDef n' sr' ty) = do
    n'' <- assertPartialNamesEqual (p1, n) (p2, n')
    pure (Both n'' (sr <> sr') ty e)
mergePartials l r = throw (DuplicateDeclaration l r)

genPartials :: [FrontendDeclaration] -> Desugar ()
genPartials = traverseOf_ (each . _Unwrapped) genPartial
  where
    genPartial :: Located FrontendDeclaration' -> Desugar ()
    genPartial (Located wholeDeclRegion (decl :: FrontendDeclaration')) =
        traverseOf_ (the @"body" . _Unwrapped . unlocated) genPartial' decl
      where
        genPartial' :: FrontendDeclarationBody' -> Desugar ()
        genPartial' db = do
            partial <- genPartial'' db
            let f = insertWithM mergePartials (IgnoreLocation (decl ^. field' @"name")) partial
            modifyM (traverseOf partialDeclarations f)

        genPartial'' :: FrontendDeclarationBody' -> Desugar PartialDeclaration
        genPartial'' (Value e pats _) = do
            exp' <- desugarExpr e
            pats' <- traverse desugarPattern pats
            let body = foldLambda pats' exp'

            pure (JustLet (decl ^. field' @"name" . unlocated :: Name) wholeDeclRegion body)
        genPartial'' (ValueTypeDef ty) = do
            ty' <- traverseOf (_Unwrapped . unlocated) desugarType ty
            pure (JustDef (decl ^. field' @"name" . unlocated) wholeDeclRegion ty')
        genPartial'' (TypeDeclaration vars typeDecl) = do
            let traverseDecl :: FrontendTypeDeclaration -> Desugar DesugaredTypeDeclaration
                traverseDecl (Alias t) = Alias <$> traverseOf (_Unwrapped . unlocated) desugarType t
                traverseDecl (ADT constructors) = ADT <$> traverseOf (each . _2 . each . _Unwrapped . unlocated) desugarType constructors
            typeDecl' <- traverseOf unlocated traverseDecl typeDecl
            let decl' = TypeDeclaration vars typeDecl'
            let bodyLoc = decl ^. the @"body" . _Unwrapped . sourceRegion
            pure (Immediate (decl ^. field' @"name" . unlocated) (DeclarationBody (Located bodyLoc decl')))

desugarType :: FrontendType' -> Desugar DesugaredType'
desugarType x = pure (unsafeCoerce x)

completePartials :: Located ModuleName -> Desugar [DesugaredDeclaration]
completePartials mn = do
    partials <- use' partialDeclarations
    decls <-
        M.traverseWithKey
            ( \declName partial -> do
                body <- resolvePartialDeclaration partial
                let locatedName = declName ^. _IgnoreLocation
                let declaration' = Declaration' mn locatedName body
                let overallLocation = locatedName ^. sourceRegion <> body ^. _Unwrapped . sourceRegion
                pure (Declaration (Located overallLocation declaration'))
            )
            partials
    pure (M.elems decls)

desugarExpr :: FrontendExpr -> Desugar DesugaredExpr
desugarExpr (Expr fe) = (\x -> Expr (x, Nothing)) <$> traverseOf unlocated desugarExpr' (fe ^. _1)
  where
    desugarExpr' :: FrontendExpr' -> Desugar DesugaredExpr'
    desugarExpr' (Int i) = pure (Int i)
    desugarExpr' (Float f) = pure (Float f)
    desugarExpr' (String s) = pure (String s)
    desugarExpr' (Char c) = pure (Char c)
    desugarExpr' Unit = pure Unit
    desugarExpr' (Var v) = pure (Var v)
    desugarExpr' (Constructor c) = pure (Constructor c)
    desugarExpr' (Lambda pats e) = do
        pats' <- traverseOf unlocated (traverse desugarPattern) pats
        e' <- desugarExpr e

        pure (foldLambda (pats' ^. unlocated) e' ^. _Unwrapped . _1 . unlocated) -- Somewhat cursed but it works
    desugarExpr' (FunctionCall e1 e2) = liftA2 FunctionCall (desugarExpr e1) (desugarExpr e2)
    desugarExpr' (TypeApplication e1 e2) = liftA2 TypeApplication (desugarExpr e1) (traverseOf (_Unwrapped . unlocated) desugarType e2)
    desugarExpr' (If a b c) = liftA3 If (desugarExpr a) (desugarExpr b) (desugarExpr c)
    desugarExpr' (BinaryOperator (o, a, b)) = liftA3 (curry3 BinaryOperator) (desugarBinaryOperator o) (desugarExpr a) (desugarExpr b)
    desugarExpr' (List e) = List <$> traverse desugarExpr e
    desugarExpr' (Match e cases) = do
        e' <- desugarExpr e
        cases' <- traverse (bitraverse desugarPattern desugarExpr) cases
        pure $ Match e' cases'
    desugarExpr' (Let n pats body) = do
        pats' <- traverse desugarPattern pats
        body' <- desugarExpr body
        pure (Let n NoFieldValue (foldLambda pats' body'))
    desugarExpr' (LetIn n pats e body) = do
        pats' <- traverse desugarPattern pats
        e' <- desugarExpr e
        body' <- desugarExpr body
        pure (LetIn n NoFieldValue (foldLambda pats' e') body')
    desugarExpr' (Block e) = Block <$> traverse desugarExpr e
    desugarExpr' (Tuple e) = Tuple <$> traverse desugarExpr e

desugarBinaryOperator :: FrontendBinaryOperator -> Desugar DesugaredBinaryOperator
desugarBinaryOperator (MkBinaryOperator lop) = MkBinaryOperator <$> traverseOf unlocated desugarBinaryOperator' lop
  where
    desugarBinaryOperator' :: FrontendBinaryOperator' -> Desugar DesugaredBinaryOperator'
    desugarBinaryOperator' (SymOp o) = pure (SymOp o)
    desugarBinaryOperator' (Infixed o) = pure (Infixed o)

desugarPattern :: FrontendPattern -> Desugar DesugaredPattern
desugarPattern (Pattern lp) =
    Pattern
        <$> bitraverse
            (traverseOf unlocated desugarPattern')
            (traverse (traverseOf (_Unwrapped . unlocated) desugarType))
            lp
  where
    desugarPattern' :: FrontendPattern' -> Desugar DesugaredPattern'
    desugarPattern' (IntegerPattern i) = pure (IntegerPattern i)
    desugarPattern' (FloatPattern f) = pure (FloatPattern f)
    desugarPattern' (StringPattern s) = pure (StringPattern s)
    desugarPattern' (CharPattern c) = pure (CharPattern c)
    desugarPattern' (VarPattern v) = pure (VarPattern v)
    desugarPattern' (ConstructorPattern c pats) = ConstructorPattern c <$> traverse desugarPattern pats
    desugarPattern' WildcardPattern = pure WildcardPattern
    desugarPattern' UnitPattern = pure UnitPattern
    desugarPattern' (ListPattern pats) = ListPattern <$> traverse desugarPattern pats
    desugarPattern' (ConsPattern a b) = liftA2 ConsPattern (desugarPattern a) (desugarPattern b)

{-

\| Lambdas need quite a lot of desugaring - they need to be unfolded into a series of nested lambdas, and then each pattern needs to be converted into a match expression.

For example, @\a (b, c) 1 -> e@ becomes
@\a -> \b_1 -> \c_1 -> match b_1 with
                            (b, c) -> match 1 with
                                        1 -> e@

However, converting the matches would require renaming, and we're not able to do that yet.
Instead, we unfold the lambda, but keep the patterns, and the renamer handles the match conversion.

-}

foldLambda :: [DesugaredPattern] -> DesugaredExpr -> DesugaredExpr
foldLambda [] e = e
foldLambda (p : ps) e =
    over
        (_Unwrapped . _1)
        (\e' -> Located (e' ^. sourceRegion) (Lambda p (foldLambda ps e)))
        e
