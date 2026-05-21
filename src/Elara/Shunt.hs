{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use fewer imports" -}

{- | This module performs "shunting", the process of rearranging binary operators in expressions to match their defined precedence and associativity.
The main meat of this module is 'fixOperators', which does the actual rearranging of operators in expressions.
The logic for this is based on https://stackoverflow.com/a/67992584/6272977, which was very helpful :).

Most of the other functions in this module are less interesting and mainly plumbing into the compiler, particularly a lot of boilerplate on traversing the AST.
-}
module Elara.Shunt (
    runGetOpInfoQuery,
    runGetOpTableInQuery,

    -- * Testing exports
    fixOperators,
    shuntExpr,
    fixExpr,
    ShuntPipelineEffects,
    OpLookup,
    HasOpLookup,
)
where

import Data.Generics.Wrapped
import Effectful (Eff, (:>))
import Effectful.Error.Static qualified as Eff
import Effectful.Writer.Static.Local qualified as Eff
import Elara.AST.Extensions (BinaryOperatorExtension (..), InParensExtension (..))
import Elara.AST.Location
import Elara.AST.Module
import Elara.AST.Name (ModuleName, Name (..), Qualified (..), VarName (..))
import Elara.AST.Phase (NoExtension (..))
import Elara.AST.PhaseCoerce (PhaseCoerce (..))
import Elara.AST.Phases.Renamed
import Elara.AST.Phases.Shunted
import Elara.AST.Region (IgnoreLocation (..), Located (..), SourceRegion (..), enclosingRegion, unlocated)
import Elara.AST.Types
import Elara.AST.VarRef
import Elara.ConstExpr
import Elara.Data.Unique (Unique (Unique))
import Elara.Error (runErrorOrReport)
import Elara.Prim (associativityAnnotationName, fixityAnnotationName, leftAssociativeAnnotationName, nonAssociativeAnnotationName, rightAssociativeAnnotationName)
import Elara.Query (Query (..), RunPhase (..))
import Elara.Query.Effects
import Elara.Rename ()
import Elara.Rename.Error (RenameError)
import Elara.Rules.Generic
import Elara.Shunt.Error
import Elara.Shunt.Operator
import Rock (Rock)
import Rock qualified
import Prelude hiding (modify')

instance PhaseCoerce (Type loc Renamed) (Type loc Shunted)

instance PhaseCoerce (Type' loc Renamed) (Type' loc Shunted)

instance PhaseCoerce (TypeDeclaration loc Renamed) (TypeDeclaration loc Shunted)

instance PhaseCoerce (Exposing loc Renamed) (Exposing loc Shunted)

instance PhaseCoerce (Exposition loc Renamed) (Exposition loc Shunted)

instance PhaseCoerce (Import loc Renamed) (Import loc Shunted)

instance PhaseCoerce (Import' loc Renamed) (Import' loc Shunted)

instance PhaseCoerce (ImportExposingOrHiding loc Renamed) (ImportExposingOrHiding loc Shunted)

{- | The default precedence for an operator if none is specified
>>> defaultPrecedence
Precedence 9
-}
defaultPrecedence :: Precedence
defaultPrecedence = mkPrecedence 9

{- | The default associativity for an operator if none is specified
>>> defaultAssociativity
LeftAssociative
-}
defaultAssociativity :: Associativity
defaultAssociativity = LeftAssociative

instance RunPhase Shunted where
    type ASTQueryEffects Shunted q = '[Eff.Writer (Set ShuntWarning), Eff.Error ShuntError]

    getModuleByName mn = do
        renamed <- runErrorOrReport @RenameError $ Rock.fetch $ Elara.Query.RenamedModule mn
        shuntWith opLookupQueries renamed

    getDeclarationByName = genericGetDeclarationByName @Shunted getModuleByName
    getRequiredDeclarationByName = genericGetRequiredDeclarationByName @Shunted getDeclarationByName
    getConstructorDeclaration = genericGetConstructorDeclaration @Shunted getModuleByName
    getDeclarationAnnotations = genericGetDeclarationAnnotations @Shunted getRequiredDeclarationByName
    getDeclarationAnnotationsOfType = genericGetDeclarationAnnotationsOfType @Shunted getDeclarationAnnotations getConstructorDeclaration

{- | A function that can lookup operator info.
This module only instantiates this function with a value that looks up operator info from the AST, but
other implementations are possible, e.g. a hardcoded table, which may be useful for primitives or testing.
-}
type OpLookup es = IgnoreLocVarRef Name -> Eff es (Maybe OpInfo)

-- | An 'OpLookup' that uses the 'Elara.Query.GetOpInfo' query to get operator info, i.e. derives it from the AST annotations.
opLookupQueries ::
    ( Eff.Error ShuntError :> es
    , Rock Query :> es
    , Eff.Writer (Set ShuntWarning) :> es
    , QueryEffects es
    ) =>
    OpLookup es
opLookupQueries name = Rock.fetch (Elara.Query.GetOpInfo name)

-- | Run the @'Elara.Query.GetOpInfo'@ query to get operator info for a given operator
runGetOpInfoQuery ::
    IgnoreLocVarRef Name ->
    Eff
        ( ConsQueryEffects
            '[ Eff.Writer (Set ShuntWarning)
             , Eff.Error ShuntError
             , Rock Elara.Query.Query
             ]
        )
        (Maybe OpInfo)
runGetOpInfoQuery (Global (IgnoreLocation locatedName@(Located _ declName))) = do
    annotations <- runErrorOrReport @RenameError $ Rock.fetch $ Elara.Query.DeclarationAnnotations @Renamed declName
    let fixityAnns = filter (\(Annotation annotName _args) -> annotName ^. unlocated == fixityAnnotationName) annotations
    let assocAnns = filter (\(Annotation annotName _args) -> annotName ^. unlocated == associativityAnnotationName) annotations
    fixity <- case fixityAnns of
        [] -> pure Nothing
        [Annotation _ [fixityArg]] ->
            case interpretNewAnnotationArg fixityArg of
                Just (ConstInt n) | n >= 0 && n <= 9 -> pure $ Just (mkPrecedence (fromInteger n))
                _invalid -> pure Nothing
        _invalid -> pure Nothing

    whenNothing_ fixity $
        Eff.tell (one (UnknownPrecedence locatedName))

    assoc <- case assocAnns of
        [] -> pure Nothing
        [Annotation lAssoc _] | lAssoc ^. unlocated == leftAssociativeAnnotationName -> pure $ Just LeftAssociative
        [Annotation rAssoc _] | rAssoc ^. unlocated == rightAssociativeAnnotationName -> pure $ Just RightAssociative
        [Annotation nAssoc _] | nAssoc ^. unlocated == nonAssociativeAnnotationName -> pure $ Just NonAssociative
        _invalid -> pure Nothing

    pure $ case (fixity, assoc) of
        (Just f, Just a) -> Just (OpInfo f a)
        (Nothing, Just a) -> Just (OpInfo defaultPrecedence a)
        (Just f, Nothing) -> Just (OpInfo f defaultAssociativity)
        (Nothing, Nothing) -> Just (OpInfo defaultPrecedence defaultAssociativity)
runGetOpInfoQuery (Local i) = Eff.throwError $ LocalOperatorInfoNotSupported (i ^. _Unwrapped)

-- | Run the @'Elara.Query.GetOpTableIn'@ query to get the operator table for a module
runGetOpTableInQuery :: ModuleName -> Eff (ConsQueryEffects '[Rock Elara.Query.Query]) OpTable
runGetOpTableInQuery _moduleName = pure mempty

-- | Effects needed for the shunt pipeline
type ShuntPipelineEffects es =
    ( QueryEffects es
    , Eff.Error ShuntError :> es
    , Eff.Writer (Set ShuntWarning) :> es
    , Rock Elara.Query.Query :> es
    )

-- | Constraint synonym for having an operator lookup in the effects (as an implicit parameter)
type HasOpLookup es = (?lookup :: OpLookup es)

-- | Shunt a renamed module using the given operator lookup function
shuntWith ::
    forall es.
    ShuntPipelineEffects es =>
    OpLookup es ->
    Module SourceRegion Renamed ->
    Eff es (Module SourceRegion Shunted)
shuntWith opL (Module loc m') = do
    declarations' <- traverse (shuntDeclaration opL) m'.moduleDeclarations
    let exposing' = phaseCoerce m'.moduleExposing
    let imports' = phaseCoerce <$> m'.moduleImports
    pure $ Module loc $ Module' m'.moduleName exposing' imports' declarations'

-- | Shunt a single declaration
shuntDeclaration ::
    forall es.
    ShuntPipelineEffects es =>
    OpLookup es ->
    Declaration SourceRegion Renamed ->
    Eff es (Declaration SourceRegion Shunted)
shuntDeclaration opL (Declaration dloc (Declaration' mn body)) = do
    body' <- shuntDeclarationBody opL body
    pure $ Declaration dloc (Declaration' mn body')

-- | Shunt a declaration body
shuntDeclarationBody ::
    forall es.
    ShuntPipelineEffects es =>
    OpLookup es ->
    DeclarationBody SourceRegion Renamed ->
    Eff es (DeclarationBody SourceRegion Shunted)
shuntDeclarationBody opL (DeclarationBody bloc body') = DeclarationBody bloc <$> go body'
  where
    go :: DeclarationBody' SourceRegion Renamed -> Eff es (DeclarationBody' SourceRegion Shunted)
    go (ValueDeclaration name val _ _ mTypeMeta anns) = do
        val' <- let ?lookup = opL in fixExpr val
        let mTypeMeta' = fmap phaseCoerce mTypeMeta
        anns' <- traverse (let ?lookup = opL in shuntAnnotation) anns
        pure $ ValueDeclaration name val' () () mTypeMeta' anns'
    go (TypeDeclarationBody name vars typeDecl mKind _meta anns) = do
        anns' <- traverse (let ?lookup = opL in shuntAnnotation) anns
        pure $ TypeDeclarationBody name vars (phaseCoerce typeDecl) (fmap phaseCoerce mKind) NoExtension anns'
    go (DeclBodyExtension v) = absurd v

-- | Shunt an annotation
shuntAnnotation :: (ShuntPipelineEffects r, HasOpLookup r) => Annotation SourceRegion Renamed -> Eff r (Annotation SourceRegion Shunted)
shuntAnnotation (Annotation name args) = do
    args' <- traverse (\(AnnotationArg e) -> AnnotationArg <$> fixExpr e) args
    pure $ Annotation name args'

{- | Fix the operators in an expression to the correct precedence and shunt it
The main entry point for this module that simply combines 'fixOperators' and 'shuntExpr'
-}
fixExpr :: (ShuntPipelineEffects r, HasOpLookup r) => RenamedExpr -> Eff r ShuntedExpr
fixExpr e = do
    fixed <- fixOperators e
    shuntExpr fixed

-- | Convert an operator to its qualified 'Name' for lookup
opNameOf (SymOp _ (TaggedLocate _ opRef)) =
    ignoreLocation (withName opRef)
opNameOf (InfixedOp _ vn) = ignoreLocation vn

{- | Fix the operators in an expression to the correct precedence.
For example given @((+) = 1l) and ((*) = 2r)@,
@1 + 2 * 3 * 4 + 5 + 6@ should be parsed as @(((1 + (2 * 3)) * 4) + 5) + 6@.
-}
fixOperators :: forall r. (ShuntPipelineEffects r, ?lookup :: OpLookup r) => RenamedExpr -> Eff r RenamedExpr
fixOperators = reassoc
  where
    reassoc :: RenamedExpr -> Eff r RenamedExpr
    reassoc (Expr loc meta (EExtension (RenamedInParens (InParensExpression e)))) = do
        e' <- reassoc e
        pure (Expr loc meta (EExtension (RenamedInParens (InParensExpression e'))))
    reassoc (Expr loc meta (EExtension (RenamedBinaryOperator (BinaryOperatorExpression operator l r)))) = do
        l' <- fixOperators l
        r' <- fixOperators r
        e' <- reassoc' (unwrapLoc loc) operator l' r'
        pure (Expr loc meta e')
    reassoc e = pure e

    reassoc' :: SourceRegion -> BinaryOperator SourceRegion Renamed -> RenamedExpr -> RenamedExpr -> Eff r RenamedExpr'
    reassoc' sr o1 e1 r@(Expr _ _ (EExtension (RenamedBinaryOperator (BinaryOperatorExpression o2 e2 e3)))) = do
        info1 <- getInfoOrWarn o1
        info2 <- getInfoOrWarn o2
        case compare info1.precedence info2.precedence of
            GT -> assocLeft
            LT -> assocRight
            EQ -> case (info1.associativity, info2.associativity) of
                (LeftAssociative, LeftAssociative) -> assocLeft
                (RightAssociative, RightAssociative) -> assocRight
                (_, _) -> Eff.throwError (SamePrecedenceError (o1, info1) (o2, info2))
      where
        assocLeft = do
            reassociated' <- reassoc' sr o1 e1 e2
            let reassociated = Expr (wrap @ExprNode sr) Nothing reassociated'
            pure (EExtension (RenamedBinaryOperator (BinaryOperatorExpression o2 reassociated e3)))

        assocRight = pure (EExtension (RenamedBinaryOperator (BinaryOperatorExpression o1 e1 r)))
    reassoc' _ operator l r = pure (EExtension (RenamedBinaryOperator (BinaryOperatorExpression operator l r)))

    getInfoOrWarn :: BinaryOperator SourceRegion Renamed -> Eff r OpInfo
    getInfoOrWarn operator = do
        info <- ?lookup (opNameOf operator)
        case info of
            Just info' -> pure info'
            Nothing -> do
                -- use default precedence 9 left associative
                pure (OpInfo (mkPrecedence 9) LeftAssociative)

{- | Shunt a renamed expression into a shunted expression.
This doesn't actually do much other than traverse the AST and convert types.
However, it does also convert binary operators into function calls.
-}
shuntExpr ::
    forall r.
    (ShuntPipelineEffects r, HasOpLookup r) =>
    RenamedExpr ->
    Eff r ShuntedExpr
shuntExpr (Expr loc meta e') = do
    (shunted, meta') <- shuntExpr' (unwrapLoc loc) e'
    pure $ Expr loc (phaseCoerce <$> meta <|> meta') shunted
  where
    shuntExpr' :: SourceRegion -> RenamedExpr' -> Eff r (ShuntedExpr', Maybe ShuntedType)
    shuntExpr' _ (EInt i) = pure (EInt i, Nothing)
    shuntExpr' _ (EFloat f) = pure (EFloat f, Nothing)
    shuntExpr' _ (EString s) = pure (EString s, Nothing)
    shuntExpr' _ (EChar c) = pure (EChar c, Nothing)
    shuntExpr' _ EUnit = pure (EUnit, Nothing)
    shuntExpr' _ (EVar NoExtension v) = pure (EVar NoExtension v, Nothing)
    shuntExpr' _ (ECon NoExtension v) = pure (ECon NoExtension v, Nothing)
    shuntExpr' _ (ELam NoExtension (TypedLambdaParam v meta') e) = do
        e' <- fixExpr e
        let meta'' = phaseCoerce <$> meta'
        pure (ELam NoExtension (TypedLambdaParam v meta'') e', Nothing)
    shuntExpr' _ (EApp NoExtension f x) = do
        f' <- fixExpr f
        x' <- fixExpr x
        pure (EApp NoExtension f' x', Nothing)
    shuntExpr' _ (ETyApp e t) = do
        e' <- fixExpr e
        pure (ETyApp e' (phaseCoerce t), Nothing)
    shuntExpr' _ (EIf cond then' else') = do
        cond' <- fixExpr cond
        then'' <- fixExpr then'
        else'' <- fixExpr else'
        pure (EIf cond' then'' else'', Nothing)
    shuntExpr' _ (EMatch e cases) = do
        e' <- fixExpr e
        cases' <- traverse (\(p, b) -> (,) <$> shuntPattern p <*> fixExpr b) cases
        pure (EMatch e' cases', Nothing)
    shuntExpr' _ (ELetIn NoExtension vn e body) = do
        e' <- fixExpr e
        body' <- fixExpr body
        pure (ELetIn NoExtension vn e' body', Nothing)
    shuntExpr' _ (ELet NoExtension vn e) = do
        e' <- fixExpr e
        pure (ELet NoExtension vn e', Nothing)
    shuntExpr' _ (EBlock es) = do
        es' <- traverse fixExpr es
        pure (EBlock es', Nothing)
    shuntExpr' _ (EAnn e t) = do
        e' <- fixExpr e
        pure (EAnn e' (phaseCoerce t), Nothing)
    shuntExpr' _ (EExtension (RenamedBinaryOperator (BinaryOperatorExpression operator l r))) = do
        -- turn the binary operator into 2 function calls
        -- (a `op` b) -> (op a) b
        l' <- fixExpr l
        r' <- fixExpr r
        let (opExpr', opLoc) = operatorToExpr operator
        let opVar = Expr (wrap @ExprNode opLoc) Nothing opExpr'
        let callLoc = enclosingRegion opLoc (unwrapLoc (exprLoc l'))
        let leftCall = Expr (wrap @ExprNode callLoc) Nothing (EApp NoExtension opVar l')
        pure (EApp NoExtension leftCall r', Nothing)
    shuntExpr' _ (EExtension (RenamedInParens (InParensExpression e))) = do
        -- Remove parens and just return the inner expression
        Expr _ _ inner <- fixExpr e
        pure (inner, Nothing)

-- | Convert an operator reference into an expression (for turning binary ops into function calls)
operatorToExpr :: BinaryOperator SourceRegion Renamed -> (ShuntedExpr', SourceRegion)
operatorToExpr (SymOp opLoc (TaggedLocate _ opRef)) =
    let varRef = case opRef of
            Global (Located l (Qualified n m)) -> Global (Located l (Qualified (OperatorVarName n) m))
            Local (Located l (Unique n i)) -> Local (Located l (Unique (OperatorVarName n) i))
     in (EVar NoExtension (TaggedLocate (wrap @VarNode opLoc) varRef), opLoc)
operatorToExpr (InfixedOp opLoc inName) =
    case inName of
        Global (Located l (Qualified (NameValue n) m)) ->
            (EVar NoExtension (TaggedLocate (wrap @VarNode opLoc) (Global (Located l (Qualified (NormalVarName n) m)))), opLoc)
        Global (Located l (Qualified (NameOp n) m)) ->
            (EVar NoExtension (TaggedLocate (wrap @VarNode opLoc) (Global (Located l (Qualified (OperatorVarName n) m)))), opLoc)
        Global (Located _ (Qualified (NameType n) m)) ->
            (ECon NoExtension (TaggedLocate (wrap @TypeNode opLoc) (Qualified n m)), opLoc)
        Local (Located l (Unique (NameValue n) i)) ->
            (EVar NoExtension (TaggedLocate (wrap @VarNode opLoc) (Local (Located l (Unique (NormalVarName n) i)))), opLoc)
        Local (Located l (Unique (NameOp n) i)) ->
            (EVar NoExtension (TaggedLocate (wrap @VarNode opLoc) (Local (Located l (Unique (OperatorVarName n) i)))), opLoc)
        Local (Located _ (Unique (NameType _) _)) -> error "Shouldn't have local con names"

-- | Get the location of an expression
exprLoc :: Expr SourceRegion p -> NodeLoc ExprNode SourceRegion
exprLoc (Expr loc _ _) = loc

-- | Shunt a pattern (trivial conversion since Renamed and Shunted patterns are structurally identical)
shuntPattern :: RenamedPattern -> Eff r ShuntedPattern
shuntPattern (Pattern loc meta p') = Pattern loc (phaseCoerce <$> meta) <$> shuntPattern' p'
  where
    shuntPattern' :: RenamedPattern' -> Eff r ShuntedPattern'
    shuntPattern' (PVar v) = pure (PVar v)
    shuntPattern' (PCon v ps) = PCon v <$> traverse shuntPattern ps
    shuntPattern' PWildcard = pure PWildcard
    shuntPattern' (PInt i) = pure (PInt i)
    shuntPattern' (PFloat f) = pure (PFloat f)
    shuntPattern' (PString s) = pure (PString s)
    shuntPattern' (PChar c) = pure (PChar c)
    shuntPattern' PUnit = pure PUnit
    shuntPattern' (PExtension v) = absurd v
