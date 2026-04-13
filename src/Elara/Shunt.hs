{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | This module performs "shunting", the process of rearranging binary operators in expressions to match their defined precedence and associativity.
The main meat of this module is 'fixOperators', which does the actual rearranging of operators in expressions.
The logic for this is based on https://stackoverflow.com/a/67992584/6272977, which was very helpful :).

Most of the other functions in this module are less interesting and mainly plumbing into the compiler, particularly a lot of boilerplate on traversing the AST.
-}
module Elara.Shunt (
    runGetOpInfoQuery,
    runGetOpTableInQuery,
    runGetShuntedModuleQuery,

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
import Effectful (Eff, inject, (:>))
import Effectful.Error.Static qualified as Eff
import Effectful.Writer.Static.Local qualified as Eff
import Elara.AST.Extensions (BinaryOperatorExtension (..), InParensExtension (..))
import Elara.AST.Module qualified as NewModule
import Elara.AST.Name (ModuleName, Name (..), Qualified (..), VarName (..))
import Elara.AST.Phase (NoExtension (..))
import Elara.AST.PhaseCoerce (PhaseCoerce (..))
import Elara.AST.Phases.Renamed (RenamedExpressionExtension (..), TypedLambdaParam (..))
import Elara.AST.Phases.Renamed qualified as NewR
import Elara.AST.Phases.Shunted qualified as NewS
import Elara.AST.Region (IgnoreLocation (..), Located (..), SourceRegion (..), enclosingRegion', unlocated)
import Elara.AST.Types qualified as New
import Elara.AST.VarRef
import Elara.ConstExpr
import Elara.Data.Unique (Unique (Unique))
import Elara.Error (ReportableError (report), runErrorOrReport)
import Elara.Prim (associativityAnnotationName, fixityAnnotationName, leftAssociativeAnnotationName, nonAssociativeAnnotationName, rightAssociativeAnnotationName)
import Elara.Query (Query (..), QueryType (..), SupportsQueries, SupportsQuery (..))
import Elara.Query.Effects
import Elara.Query.Errors
import Elara.Rename ()
import Elara.Rename.Error (RenameError)
import Elara.Rules.Generic ()
import Elara.Shunt.Error
import Elara.Shunt.Operator
import Rock (Rock)
import Rock qualified
import Prelude hiding (modify')

instance PhaseCoerce (New.Type loc NewR.Renamed) (New.Type loc NewS.Shunted)

instance PhaseCoerce (New.Type' loc NewR.Renamed) (New.Type' loc NewS.Shunted)

instance PhaseCoerce (New.TypeDeclaration loc NewR.Renamed) (New.TypeDeclaration loc NewS.Shunted)

instance PhaseCoerce (NewModule.Exposing loc NewR.Renamed) (NewModule.Exposing loc NewS.Shunted)

instance PhaseCoerce (NewModule.Exposition loc NewR.Renamed) (NewModule.Exposition loc NewS.Shunted)

instance PhaseCoerce (NewModule.Import loc NewR.Renamed) (NewModule.Import loc NewS.Shunted)

instance PhaseCoerce (NewModule.Import' loc NewR.Renamed) (NewModule.Import' loc NewS.Shunted)

instance PhaseCoerce (NewModule.ImportExposingOrHiding loc NewR.Renamed) (NewModule.ImportExposingOrHiding loc NewS.Shunted)

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

instance SupportsQuery QueryModuleByName NewS.Shunted where
    type QuerySpecificEffectsOf QueryModuleByName NewS.Shunted = StandardQueryError NewS.Shunted
    query mn = do
        (mod', warnings) <- Eff.runWriter $ inject $ runGetShuntedModuleQuery mn
        traverse_ report warnings
        pure mod'

{- | A function that can lookup operator info.
This module only instantiates this function with a value that looks up operator info from the AST, but
other implementations are possible, e.g. a hardcoded table, which may be useful for primitives or testing.
-}
type OpLookup es = IgnoreLocVarRef Name -> Eff es (Maybe OpInfo)

-- | Run the @'Elara.Query.QueryModuleByName' 'Shunted'@ query, which shunts a renamed module
runGetShuntedModuleQuery ::
    ModuleName ->
    Eff
        ( ConsQueryEffects
            '[ Eff.Error ShuntError
             , Eff.Writer (Set ShuntWarning)
             , Rock Elara.Query.Query
             ]
        )
        (NewModule.Module SourceRegion NewS.Shunted)
runGetShuntedModuleQuery mn = do
    renamed <- runErrorOrReport @RenameError $ Rock.fetch $ Elara.Query.RenamedModule mn
    shuntWith opLookupQueries renamed

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
    SupportsQueries [QueryDeclarationByName, DeclarationAnnotations, QueryConstructorDeclaration] NewR.Renamed =>
    IgnoreLocVarRef Name ->
    Eff
        ( ConsQueryEffects
            '[ Eff.Writer (Set ShuntWarning)
             , Eff.Error ShuntError
             , Rock Elara.Query.Query
             ]
        )
        (Maybe OpInfo)
runGetOpInfoQuery (Global (IgnoreLocation (Located _ declName))) = do
    fixityAnns <- runErrorOrReport @RenameError $ Rock.fetch $ Elara.Query.DeclarationAnnotationsOfType @NewR.Renamed (declName, fixityAnnotationName)
    assocAnns <- runErrorOrReport @RenameError $ Rock.fetch $ Elara.Query.DeclarationAnnotationsOfType @NewR.Renamed (declName, associativityAnnotationName)
    fixity <- case fixityAnns of
        [] -> pure Nothing
        [New.Annotation _ [fixityArg]] ->
            case interpretNewAnnotationArg fixityArg of
                Just (ConstInt n) | n >= 0 && n <= 9 -> pure $ Just (mkPrecedence (fromInteger n))
                _invalid -> pure Nothing
        _invalid -> pure Nothing

    assoc <- case assocAnns of
        [] -> pure Nothing
        [New.Annotation lAssoc _] | lAssoc ^. unlocated == leftAssociativeAnnotationName -> pure $ Just LeftAssociative
        [New.Annotation rAssoc _] | rAssoc ^. unlocated == rightAssociativeAnnotationName -> pure $ Just RightAssociative
        [New.Annotation nAssoc _] | nAssoc ^. unlocated == nonAssociativeAnnotationName -> pure $ Just NonAssociative
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
    NewModule.Module SourceRegion NewR.Renamed ->
    Eff es (NewModule.Module SourceRegion NewS.Shunted)
shuntWith opL (NewModule.Module loc m') = do
    declarations' <- traverse (shuntDeclaration opL) m'.moduleDeclarations
    let exposing' = phaseCoerce m'.moduleExposing
    let imports' = phaseCoerce <$> m'.moduleImports
    pure $ NewModule.Module loc $ NewModule.Module' m'.moduleName exposing' imports' declarations'

-- | Shunt a single declaration
shuntDeclaration ::
    forall es.
    ShuntPipelineEffects es =>
    OpLookup es ->
    New.Declaration SourceRegion NewR.Renamed ->
    Eff es (New.Declaration SourceRegion NewS.Shunted)
shuntDeclaration opL (New.Declaration dloc (New.Declaration' mn body)) = do
    body' <- shuntDeclarationBody opL body
    pure $ New.Declaration dloc (New.Declaration' mn body')

-- | Shunt a declaration body
shuntDeclarationBody ::
    forall es.
    ShuntPipelineEffects es =>
    OpLookup es ->
    New.DeclarationBody SourceRegion NewR.Renamed ->
    Eff es (New.DeclarationBody SourceRegion NewS.Shunted)
shuntDeclarationBody opL (New.DeclarationBody bloc body') = New.DeclarationBody bloc <$> go body'
  where
    go :: New.DeclarationBody' SourceRegion NewR.Renamed -> Eff es (New.DeclarationBody' SourceRegion NewS.Shunted)
    go (New.ValueDeclaration name val _ _ mTypeMeta anns) = do
        val' <- let ?lookup = opL in fixExpr val
        let mTypeMeta' = fmap phaseCoerce mTypeMeta
        anns' <- traverse (let ?lookup = opL in shuntAnnotation) anns
        pure $ New.ValueDeclaration name val' () () mTypeMeta' anns'
    go (New.TypeDeclarationBody name vars typeDecl mKind _meta anns) = do
        anns' <- traverse (let ?lookup = opL in shuntAnnotation) anns
        pure $ New.TypeDeclarationBody name vars (phaseCoerce typeDecl) (fmap phaseCoerce mKind) NoExtension anns'
    go (New.DeclBodyExtension v) = absurd v

-- | Shunt an annotation
shuntAnnotation :: (ShuntPipelineEffects r, HasOpLookup r) => New.Annotation SourceRegion NewR.Renamed -> Eff r (New.Annotation SourceRegion NewS.Shunted)
shuntAnnotation (New.Annotation name args) = do
    args' <- traverse (\(New.AnnotationArg e) -> New.AnnotationArg <$> fixExpr e) args
    pure $ New.Annotation name args'

{- | Fix the operators in an expression to the correct precedence and shunt it
The main entry point for this module that simply combines 'fixOperators' and 'shuntExpr'
-}
fixExpr :: (ShuntPipelineEffects r, HasOpLookup r) => NewR.RenamedExpr -> Eff r NewS.ShuntedExpr
fixExpr e = do
    fixed <- fixOperators e
    shuntExpr fixed

-- | Convert an operator to its qualified 'Name' for lookup
opNameOf (New.SymOp _ (Located _ opRef)) =
    case opRef of
        Global (Located l (Qualified n m)) -> Global (IgnoreLocation (Located l (Qualified (NameOp n) m)))
        Local (Located l (Unique n i)) -> Local (IgnoreLocation (Located l (Unique (NameOp n) i)))
opNameOf (New.InfixedOp _ vn) = ignoreLocation vn

{- | Fix the operators in an expression to the correct precedence.
For example given @((+) = 1l) and ((*) = 2r)@,
@1 + 2 * 3 * 4 + 5 + 6@ should be parsed as @(((1 + (2 * 3)) * 4) + 5) + 6@.
-}
fixOperators :: forall r. (ShuntPipelineEffects r, ?lookup :: OpLookup r) => NewR.RenamedExpr -> Eff r NewR.RenamedExpr
fixOperators = reassoc
  where
    reassoc :: NewR.RenamedExpr -> Eff r NewR.RenamedExpr
    reassoc (New.Expr loc meta (New.EExtension (RenamedInParens (InParensExpression e)))) = do
        e' <- reassoc e
        pure (New.Expr loc meta (New.EExtension (RenamedInParens (InParensExpression e'))))
    reassoc (New.Expr loc meta (New.EExtension (RenamedBinaryOperator (BinaryOperatorExpression operator l r)))) = do
        l' <- fixOperators l
        r' <- fixOperators r
        e' <- reassoc' loc operator l' r'
        pure (New.Expr loc meta e')
    reassoc e = pure e

    reassoc' :: SourceRegion -> New.BinaryOperator SourceRegion NewR.Renamed -> NewR.RenamedExpr -> NewR.RenamedExpr -> Eff r NewR.RenamedExpr'
    reassoc' sr o1 e1 r@(New.Expr _ _ (New.EExtension (RenamedBinaryOperator (BinaryOperatorExpression o2 e2 e3)))) = do
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
            let reassociated = New.Expr sr Nothing reassociated'
            pure (New.EExtension (RenamedBinaryOperator (BinaryOperatorExpression o2 reassociated e3)))

        assocRight = pure (New.EExtension (RenamedBinaryOperator (BinaryOperatorExpression o1 e1 r)))
    reassoc' _ operator l r = pure (New.EExtension (RenamedBinaryOperator (BinaryOperatorExpression operator l r)))

    getInfoOrWarn :: New.BinaryOperator SourceRegion NewR.Renamed -> Eff r OpInfo
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
    NewR.RenamedExpr ->
    Eff r NewS.ShuntedExpr
shuntExpr (New.Expr loc meta e') = do
    (shunted, meta') <- shuntExpr' loc e'
    pure $ New.Expr loc (phaseCoerce <$> meta <|> meta') shunted
  where
    shuntExpr' :: SourceRegion -> NewR.RenamedExpr' -> Eff r (NewS.ShuntedExpr', Maybe NewS.ShuntedType)
    shuntExpr' _ (New.EInt i) = pure (New.EInt i, Nothing)
    shuntExpr' _ (New.EFloat f) = pure (New.EFloat f, Nothing)
    shuntExpr' _ (New.EString s) = pure (New.EString s, Nothing)
    shuntExpr' _ (New.EChar c) = pure (New.EChar c, Nothing)
    shuntExpr' _ New.EUnit = pure (New.EUnit, Nothing)
    shuntExpr' _ (New.EVar NoExtension v) = pure (New.EVar NoExtension v, Nothing)
    shuntExpr' _ (New.ECon NoExtension v) = pure (New.ECon NoExtension v, Nothing)
    shuntExpr' _ (New.ELam NoExtension (TypedLambdaParam v meta') e) = do
        e' <- fixExpr e
        let meta'' = phaseCoerce <$> meta'
        pure (New.ELam NoExtension (TypedLambdaParam v meta'') e', Nothing)
    shuntExpr' _ (New.EApp NoExtension f x) = do
        f' <- fixExpr f
        x' <- fixExpr x
        pure (New.EApp NoExtension f' x', Nothing)
    shuntExpr' _ (New.ETyApp e t) = do
        e' <- fixExpr e
        pure (New.ETyApp e' (phaseCoerce t), Nothing)
    shuntExpr' _ (New.EIf cond then' else') = do
        cond' <- fixExpr cond
        then'' <- fixExpr then'
        else'' <- fixExpr else'
        pure (New.EIf cond' then'' else'', Nothing)
    shuntExpr' _ (New.EMatch e cases) = do
        e' <- fixExpr e
        cases' <- traverse (\(p, b) -> (,) <$> shuntPattern p <*> fixExpr b) cases
        pure (New.EMatch e' cases', Nothing)
    shuntExpr' _ (New.ELetIn NoExtension vn e body) = do
        e' <- fixExpr e
        body' <- fixExpr body
        pure (New.ELetIn NoExtension vn e' body', Nothing)
    shuntExpr' _ (New.ELet NoExtension vn e) = do
        e' <- fixExpr e
        pure (New.ELet NoExtension vn e', Nothing)
    shuntExpr' _ (New.EBlock es) = do
        es' <- traverse fixExpr es
        pure (New.EBlock es', Nothing)
    shuntExpr' _ (New.EAnn e t) = do
        e' <- fixExpr e
        pure (New.EAnn e' (phaseCoerce t), Nothing)
    shuntExpr' _ (New.EExtension (RenamedBinaryOperator (BinaryOperatorExpression operator l r))) = do
        -- turn the binary operator into 2 function calls
        -- (a `op` b) -> (op a) b
        l' <- fixExpr l
        r' <- fixExpr r
        let (opExpr', opLoc) = operatorToExpr operator
        let opVar = New.Expr opLoc Nothing opExpr'
        let callLoc = enclosingRegion' opLoc (exprLoc l')
        let leftCall = New.Expr callLoc Nothing (New.EApp NoExtension opVar l')
        pure (New.EApp NoExtension leftCall r', Nothing)
    shuntExpr' _ (New.EExtension (RenamedInParens (InParensExpression e))) = do
        -- Remove parens and just return the inner expression
        New.Expr _ _ inner <- fixExpr e
        pure (inner, Nothing)

-- | Convert an operator reference into an expression (for turning binary ops into function calls)
operatorToExpr :: New.BinaryOperator SourceRegion NewR.Renamed -> (NewS.ShuntedExpr', SourceRegion)
operatorToExpr (New.SymOp opLoc (Located _ opRef)) =
    let varRef = case opRef of
            Global (Located l (Qualified n m)) ->
                Located l (Global (Located l (Qualified (OperatorVarName n) m)))
            Local (Located l (Unique n i)) ->
                Located l (Local (Located l (Unique (OperatorVarName n) i)))
     in (New.EVar NoExtension varRef, opLoc)
operatorToExpr (New.InfixedOp opLoc inName) =
    case inName of
        Global (Located l (Qualified (NameValue n) m)) ->
            (New.EVar NoExtension (Located l (Global (Located l (Qualified (NormalVarName n) m)))), opLoc)
        Global (Located l (Qualified (NameOp n) m)) ->
            (New.EVar NoExtension (Located l (Global (Located l (Qualified (OperatorVarName n) m)))), opLoc)
        Global (Located l (Qualified (NameType n) m)) ->
            (New.ECon NoExtension (Located l (Qualified n m)), opLoc)
        Local (Located l (Unique (NameValue n) i)) ->
            (New.EVar NoExtension (Located l (Local (Located l (Unique (NormalVarName n) i)))), opLoc)
        Local (Located l (Unique (NameOp n) i)) ->
            (New.EVar NoExtension (Located l (Local (Located l (Unique (OperatorVarName n) i)))), opLoc)
        Local (Located _ (Unique (NameType _) _)) -> error "Shouldn't have local con names"

-- | Get the location of an expression
exprLoc :: New.Expr SourceRegion p -> SourceRegion
exprLoc (New.Expr loc _ _) = loc

-- | Shunt a pattern (trivial conversion since Renamed and Shunted patterns are structurally identical)
shuntPattern :: NewR.RenamedPattern -> Eff r NewS.ShuntedPattern
shuntPattern (New.Pattern loc meta p') = New.Pattern loc (phaseCoerce <$> meta) <$> shuntPattern' p'
  where
    shuntPattern' :: NewR.RenamedPattern' -> Eff r NewS.ShuntedPattern'
    shuntPattern' (New.PVar v) = pure (New.PVar v)
    shuntPattern' (New.PCon v ps) = New.PCon v <$> traverse shuntPattern ps
    shuntPattern' New.PWildcard = pure New.PWildcard
    shuntPattern' (New.PInt i) = pure (New.PInt i)
    shuntPattern' (New.PFloat f) = pure (New.PFloat f)
    shuntPattern' (New.PString s) = pure (New.PString s)
    shuntPattern' (New.PChar c) = pure (New.PChar c)
    shuntPattern' New.PUnit = pure New.PUnit
    shuntPattern' (New.PExtension v) = absurd v
