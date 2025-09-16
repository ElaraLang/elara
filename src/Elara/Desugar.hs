{-# LANGUAGE TemplateHaskell #-}

module Elara.Desugar where

import Data.Generics.Product (field', the)
import Data.Generics.Wrapped
import Data.Map qualified as M
import Effectful (Eff, inject)
import Effectful.Error.Static (throwError)
import Effectful.Error.Static qualified as Eff
import Effectful.State.Extra
import Effectful.State.Static.Local qualified as Eff
import Elara.AST.Desugared
import Elara.AST.Frontend
import Elara.AST.Generic
import Elara.AST.Generic.Common
import Elara.AST.Module
import Elara.AST.Name hiding (name)
import Elara.AST.Region
import Elara.AST.Select
import Elara.Data.Pretty (Pretty (pretty))
import Elara.Desugar.Error
import Elara.Error (runErrorOrReport)
import Elara.Parse.Error (WParseErrorBundle (WParseErrorBundle))
import Elara.Query qualified
import Elara.Query.Effects (ConsQueryEffects)
import Elara.Utils (curry3)
import Optics (traverseOf_)
import Rock qualified
import Rock.Memo (MemoQuery (MemoQuery))
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (Op)

type Desugar a = Eff DesugarPipelineEffects a

type DesugarPipelineEffects = '[Eff.State DesugarState, Eff.Error DesugarError]

newtype DesugarState = DesugarState
    { _partialDeclarations :: Map (IgnoreLocation Name) PartialDeclaration
    }
    deriving (Show, Pretty, Semigroup, Monoid)

makeLenses ''DesugarState

resolvePartialDeclaration :: PartialDeclaration -> Desugar DesugaredDeclarationBody
resolvePartialDeclaration (Immediate _ a) = pure a
resolvePartialDeclaration ((JustDef _ _ ty _)) = throwError (DefWithoutLet ty)
resolvePartialDeclaration ((JustLet n sr e ann)) = pure (DeclarationBody (Located sr (Value n e NoFieldValue Nothing (resolveAnn ann))))
resolvePartialDeclaration ((AllDecl n sr ty e ann)) =
    pure
        ( DeclarationBody
            (Located sr (Value n e NoFieldValue (Just ty) ann))
        )
resolvePartialDeclaration (JustInfix n sr v) = throwError (InfixWithoutDeclaration n sr v)

resolveAnn :: Maybe (ValueDeclAnnotations Desugared) -> ValueDeclAnnotations Desugared
resolveAnn = fromMaybe (ValueDeclAnnotations Nothing)

getDesugaredModule ::
    ModuleName ->
    Eff
        (ConsQueryEffects '[Eff.Error DesugarError, Rock.Rock Elara.Query.Query])
        (Module 'Desugared)
getDesugaredModule mn = do
    parsed <- runErrorOrReport @(WParseErrorBundle _ _) $ Rock.fetch $ Elara.Query.ParsedModule mn

    inject $ Eff.evalState (DesugarState mempty) $ desugar parsed

desugar ::
    Module 'Frontend ->
    Desugar (Module 'Desugared)
desugar =
    traverseOf
        (_Unwrapped % unlocated)
        ( \(m' :: Module' 'Frontend) -> do
            let decls = m' ^. field' @"declarations" :: [FrontendDeclaration]
            decls' <- desugarDeclarations (m' ^. the @(Located ModuleName)) decls

            pure (Module' (m' ^. field' @"name") (coerceExposing (m' ^. field' @"exposing")) (coerceImport <$> m' ^. field' @"imports") decls')
        )

desugarDeclarations :: Located ModuleName -> [FrontendDeclaration] -> Desugar [DesugaredDeclaration]
desugarDeclarations mn decls = do
    genPartials decls
    completePartials mn

assertPartialNamesEqual :: Eq a => (PartialDeclaration, Located a) -> (PartialDeclaration, Located a) -> Desugar ()
assertPartialNamesEqual (p1, n1) (p2, n2) = if n1 ^. unlocated == n2 ^. unlocated then pass else throwError (PartialNamesNotEqual p1 p2)

resolveDupeInfixes :: Maybe (ValueDeclAnnotations Desugared) -> Maybe (ValueDeclAnnotations Desugared) -> Desugar (ValueDeclAnnotations Desugared)
resolveDupeInfixes (Just a@(ValueDeclAnnotations (Just _))) (Just b@(ValueDeclAnnotations (Just _))) = throwError (DuplicateAnnotations a b)
resolveDupeInfixes (Just (ValueDeclAnnotations a)) (Just (ValueDeclAnnotations b)) = pure (ValueDeclAnnotations (a <|> b))
resolveDupeInfixes a b = pure (fromMaybe (ValueDeclAnnotations Nothing) (a <|> b))

mergePartials :: PartialDeclaration -> PartialDeclaration -> Desugar PartialDeclaration
mergePartials p1@(JustInfix n sr i) p2@(JustDef n' sr' ty Nothing) = do
    assertPartialNamesEqual (p1, n) (p2, NVarName <$> n')
    pure (JustDef n' (sr <> sr') ty (Just i))
mergePartials p2@(JustDef n' sr' ty Nothing) p1@(JustInfix n sr i) = do
    assertPartialNamesEqual (p1, n) (p2, NVarName <$> n')
    pure (JustDef n' (sr <> sr') ty (Just i))
mergePartials p1@(JustInfix n sr i) p2@(JustLet n' sr' ty Nothing) = do
    assertPartialNamesEqual (p1, n) (p2, NVarName <$> n')
    pure (JustLet n' (sr <> sr') ty (Just i))
mergePartials p2@(JustLet n' sr' ty Nothing) p1@(JustInfix n sr i) = do
    assertPartialNamesEqual (p1, n) (p2, NVarName <$> n')
    pure (JustLet n' (sr <> sr') ty (Just i))
mergePartials p1@(JustDef n sr ty mAnn) p2@(JustLet n' sr' e mAnn') = do
    assertPartialNamesEqual (p1, n) (p2, n')
    ann <- resolveDupeInfixes mAnn mAnn'
    pure (AllDecl n' (sr <> sr') ty e ann)
mergePartials p1@(JustLet n sr e mAnn) p2@(JustDef n' sr' ty mAnn') = do
    assertPartialNamesEqual (p1, n) (p2, n')
    ann <- resolveDupeInfixes mAnn mAnn'
    pure (AllDecl n' (sr <> sr') ty e ann)
mergePartials l r = throwError (DuplicateDeclaration l r)

genPartials :: [FrontendDeclaration] -> Desugar ()
genPartials = traverseOf_ (each % _Unwrapped) genPartial
  where
    genPartial :: Located FrontendDeclaration' -> Desugar ()
    genPartial (Located wholeDeclRegion (decl :: FrontendDeclaration')) =
        traverseOf_ (the @"body" % _Unwrapped % unlocated) genPartial' decl
      where
        genPartial' :: FrontendDeclarationBody' -> Desugar ()
        genPartial' db = do
            partial <- genPartial'' db
            let f = insertWithM mergePartials (IgnoreLocation (db ^. declarationBody'Name)) partial
            let
                modifyM f = Eff.get >>= (Eff.put <=< f)
            modifyM (traverseOf partialDeclarations f)

        genPartial'' :: FrontendDeclarationBody' -> Desugar PartialDeclaration
        genPartial'' (InfixDecl (InfixDeclaration n p a)) = do
            let infix'' = ValueDeclAnnotations (Just (InfixDeclaration n p a))

            pure (JustInfix n wholeDeclRegion infix'')
        genPartial'' (Value n e pats _ valueAnnotations) = do
            exp' <- desugarExpr e
            pats' <- traverse desugarPattern pats
            let body = foldLambda pats' exp'
            let ann = coerceValueDeclAnnotations @Frontend @Desugared valueAnnotations

            pure (JustLet n wholeDeclRegion body (Just ann))
        genPartial'' (ValueTypeDef n ty) = do
            ty' <- traverseOf (_Unwrapped % _1 % unlocated) desugarType ty
            pure (JustDef n wholeDeclRegion ty' Nothing)
        genPartial'' (TypeDeclaration n vars typeDecl typeAnnotations) = do
            let traverseDecl :: FrontendTypeDeclaration -> Desugar DesugaredTypeDeclaration
                traverseDecl (Alias t) = Alias <$> traverseOf (_Unwrapped % _1 % unlocated) desugarType t
                traverseDecl (ADT constructors) = ADT <$> traverseOf (each % _2 % each % _Unwrapped % _1 % unlocated) desugarType constructors
            typeDecl' <- traverseOf unlocated traverseDecl typeDecl
            let ann = coerceTypeDeclAnnotations @Frontend @Desugared typeAnnotations
            let decl' = TypeDeclaration n vars typeDecl' ann
            let bodyLoc = decl ^. the @"body" % _Unwrapped % sourceRegion
            pure (Immediate (n ^. unlocated % to NTypeName) (DeclarationBody (Located bodyLoc decl')))

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
                let declaration' = Declaration' mn body
                let overallLocation = locatedName ^. sourceRegion <> body ^. _Unwrapped % sourceRegion
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

        pure (foldLambda (pats' ^. unlocated) e' ^. _Unwrapped % _1 % unlocated) -- Somewhat cursed but it works
    desugarExpr' (FunctionCall e1 e2) = liftA2 FunctionCall (desugarExpr e1) (desugarExpr e2)
    desugarExpr' (TypeApplication e1 e2) = liftA2 TypeApplication (desugarExpr e1) (traverseOf (_Unwrapped % _1 % unlocated) desugarType e2)
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
    desugarExpr' (InParens e) = InParens <$> desugarExpr e

desugarBinaryOperator :: FrontendBinaryOperator -> Desugar DesugaredBinaryOperator
desugarBinaryOperator (MkBinaryOperator lop) = MkBinaryOperator <$> traverseOf unlocated desugarBinaryOperator' lop
  where
    desugarBinaryOperator' :: FrontendBinaryOperator' -> Desugar DesugaredBinaryOperator'
    desugarBinaryOperator' (SymOp o) = pure (SymOp o)
    desugarBinaryOperator' (Infixed o) = pure (Infixed o)

desugarPattern :: FrontendPattern -> Desugar DesugaredPattern
desugarPattern p@(Pattern lp) =
    Pattern
        <$> bitraverse
            (traverseOf unlocated desugarPattern')
            (traverse (traverseOf (_Unwrapped % _1 % unlocated) desugarType))
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
    desugarPattern' (ConsPattern as) = ConsPattern <$> traverseOf each desugarPattern as
    desugarPattern' (TuplePattern (_ :| [])) = throwError (TuplePatternTooShort p)
    desugarPattern' (TuplePattern (p :| ps)) = do
        p' <- desugarPattern p
        ps' <- traverse desugarPattern ps
        pure (TuplePattern (p' :| ps'))

{- | Lambdas need quite a lot of desugaring - they need to be unfolded into a series of nested lambdas, and then each pattern needs to be converted into a match expression.

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
        (_Unwrapped % _1)
        (\e' -> Located (e' ^. sourceRegion) (Lambda p (foldLambda ps e)))
        e
