{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | This module performs "shunting", the process of rearranging binary operators in expressions to match their defined precedence and associativity.
The main meat of this module is 'fixOperators', which does the actual rearranging of operators in expressions.
The logic for this is based on https://stackoverflow.com/a/67992584/6272977, which was very helpful :).

Most of the other functions in this module are less interesting and mainly plumbing into the compiler, particularly a lot of boilerplate on traversing the AST.
-}
module Elara.Shunt (runGetOpInfoQuery, runGetOpTableInQuery) where

import Data.Generics.Product (HasField' (field'))
import Data.Generics.Wrapped
import Effectful (Eff, inject, (:>))
import Effectful.Error.Static qualified as Eff
import Effectful.State.Static.Local (execState)
import Effectful.Writer.Static.Local qualified as Eff
import Elara.AST.Generic
import Elara.AST.Generic.Common
import Elara.AST.Introspection
import Elara.AST.Module
import Elara.AST.Name (ModuleName, Name (..), Qualified (..), VarName (..), VarOrConName (..))
import Elara.AST.Region (IgnoreLocation (..), Located (..), SourceRegion, sourceRegion, unlocated, withLocationOf)
import Elara.AST.Region qualified as Located
import Elara.AST.Renamed
import Elara.AST.Select
import Elara.AST.Shunted
import Elara.AST.VarRef
import Elara.ConstExpr
import Elara.Data.Pretty
import Elara.Data.Unique (Unique (Unique))
import Elara.Error (DiagnosticWriter, ReportableError (report), runErrorOrReport)
import Elara.Prim (associativityAnnotationName, fixityAnnotationName, leftAssociativeAnnotationName, nonAssociativeAnnotationName, rightAssociativeAnnotationName)
import Elara.Query (Query (..), QueryType (..), SupportsQueries, SupportsQuery (..))
import Elara.Query.Effects
import Elara.Query.Errors
import Elara.Rename.Error (RenameError)
import Elara.Rules.Generic ()
import Elara.Shunt.Error
import Elara.Shunt.Operator
import Print (debugPretty)
import Rock (Rock, fetch)
import TODO (todo)
import Prelude hiding (modify')

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

instance SupportsQuery QueryModuleByName Shunted where
    type QuerySpecificEffectsOf QueryModuleByName Shunted = StandardQueryError Shunted
    query mn = do
        (mod, warnings) <- Eff.runWriter $ inject $ runGetShuntedModuleQuery mn
        traverse_ report warnings
        pure mod

instance IntrospectableAnnotations Shunted where
    getAnnotations a = a

{- | A function that can lookup operator info.
This module only instantiates this function with a value that looks up operator info from the AST, but
other implementations are possible, e.g. a hardcoded table, which may be useful for primitives or testing.
-}
type OpLookup es = IgnoreLocVarRef Name -> Eff es (Maybe OpInfo)

pattern InExpr :: RenamedExpr' -> RenamedExpr
pattern InExpr y <- Expr (Located _ y, _)

pattern InExpr' :: SourceRegion -> RenamedExpr' -> RenamedExpr
pattern InExpr' loc y <- Expr (Located loc y, _)

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
        (Module Shunted)
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
    SupportsQueries [QueryDeclarationByName, DeclarationAnnotations, QueryConstructorDeclaration] Renamed =>
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
    fixityAnns <- runErrorOrReport @RenameError $ Rock.fetch $ Elara.Query.DeclarationAnnotationsOfType @Renamed (declName, fixityAnnotationName)
    assocAnns <- runErrorOrReport @RenameError $ Rock.fetch $ Elara.Query.DeclarationAnnotationsOfType @Renamed (declName, associativityAnnotationName)
    -- It may be intuitive that if we received an ill-formed annotation, eg
    -- @#Fixity "f"@ that we would throw an error
    -- however, we instead choose to just ignore the annotation
    -- and let the type checker handle this later on
    -- otherwise it we have to do a "mini-type-check" here to validate the annotation
    fixity <- case fixityAnns of
        [] -> pure Nothing
        [Annotation _ [fixityArg]] -> do
            i <- interpretAnnotationArg fixityArg
            case i of
                ConstInt n | n >= 0 && n <= 9 -> pure $ Just (mkPrecedence (fromInteger n))
                _invalid -> pure Nothing
        _invalid -> pure Nothing

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
runGetOpInfoQuery (Local i) = do
    Eff.throwError $ LocalOperatorInfoNotSupported (i ^. _Unwrapped)

-- | Run the @'Elara.Query.GetOpTableIn'@ query to get the operator table for a module
runGetOpTableInQuery :: ModuleName -> Eff (ConsQueryEffects '[Rock Elara.Query.Query]) OpTable
runGetOpTableInQuery moduleName = do
    mod <- runErrorOrReport @RenameError $ Rock.fetch $ Elara.Query.RenamedModule moduleName
    createOpTable mod

{- | Create a operator table from a renamed module.
This scans all declarations in the module for fixity and associativity annotations.
-}
createOpTable :: Module Renamed -> Eff es OpTable
createOpTable = execState mempty . traverseModule_ addDeclsToOpTable'

{- | Add declarations to an operator table.
This function is incomplete and does not do anything useful yet.
-}
addDeclsToOpTable' :: Declaration Renamed -> Eff r ()
addDeclsToOpTable' (Declaration (Located _ decl)) = case decl ^. field' @"body" % _Unwrapped % unlocated of
    Value{_valueName, _valueAnnotations = (ValueDeclAnnotations a)} -> do
        debugPretty $ "No fixity for value declaration: " <> pretty _valueName
    TypeDeclaration name _ _ (TypeDeclAnnotations NoFieldValue a) ->
        -- let nameRef = Global $ IgnoreLocation (NTypeName <<$>> name)
        --  in modify $ Map.insert nameRef (infixDeclToOpInfo fixity)
        todo

-- | Convert an operator to its qualified 'Name' for lookup
opNameOf :: RenamedBinaryOperator -> IgnoreLocVarRef Name
opNameOf operator =
    case operator ^. _Unwrapped % unlocated of
        SymOp opName -> ignoreLocation (NVarName . OperatorVarName <$> opName ^. unlocated)
        Infixed vn -> ignoreLocation (toName <$> vn)
          where
            toName (VarName n) = NVarName (NormalVarName n)
            toName (ConName n) = NTypeName n

{- | Fix the operators in an expression to the correct precedence.
For example given @((+) = 1l) and ((*) = 2r)@,
@1 + 2 * 3 * 4 + 5 + 6@ should be parsed as @(((1 + (2 * 3)) * 4) + 5) + 6@.
-}
fixOperators :: forall r. (ShuntPipelineEffects r, ?lookup :: OpLookup r) => RenamedExpr -> Eff r RenamedExpr
fixOperators = reassoc
  where
    withLocationOf' :: RenamedExpr -> RenamedExpr' -> RenamedExpr
    withLocationOf' s repl = over (_Unwrapped % _1) (repl <$) s

    reassoc :: RenamedExpr -> Eff r RenamedExpr
    reassoc (Expr (Located l (InParens e), t)) = do
        e' <- reassoc e
        pure (Expr (Located l (InParens e'), t))
    reassoc e@(InExpr' loc (BinaryOperator (operator, l, r))) = do
        l' <- fixOperators l
        r' <- fixOperators r
        withLocationOf' e <$> reassoc' loc operator l' r'
    reassoc e = pure e

    reassoc' :: SourceRegion -> RenamedBinaryOperator -> RenamedExpr -> RenamedExpr -> Eff r RenamedExpr'
    reassoc' sr o1 e1 r@(InExpr (BinaryOperator (o2, e2, e3))) = do
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
            let reassociated = Expr (Located sr reassociated', Nothing)
            pure (BinaryOperator (o2, reassociated, e3))

        assocRight = pure (BinaryOperator (o1, e1, r))

        getInfoOrWarn :: RenamedBinaryOperator -> Eff r OpInfo
        getInfoOrWarn operator = do
            info <- ?lookup (opNameOf operator)
            case info of
                Just info -> pure info
                Nothing -> do
                    -- use default precedence 9 left associative
                    pure (OpInfo (mkPrecedence 9) LeftAssociative)
    reassoc' _ operator l r = pure (BinaryOperator (operator, l, r))

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
    -- | Operator lookup function
    OpLookup es ->
    -- | Renamed module to shunt
    Module Renamed ->
    Eff es (Module Shunted)
shuntWith opL = traverseModule (shuntWithDeclaration opL)

-- | Shunt a renamed declaration using the given operator lookup function
shuntWithDeclaration ::
    forall es.
    ShuntPipelineEffects es =>
    -- | Operator lookup function
    OpLookup es ->
    -- | Renamed declaration to shunt
    RenamedDeclaration ->
    Eff es ShuntedDeclaration
shuntWithDeclaration opL (Declaration decl) =
    Declaration
        <$> traverseOf
            unlocated
            ( \(decl' :: RenamedDeclaration') -> do
                body' <- shuntDeclarationBody opL (decl' ^. field' @"body")
                pure (Declaration' (decl' ^. field' @"moduleName") body')
            )
            decl

-- | Shunt a renamed declaration body using the given operator lookup function
shuntDeclarationBody ::
    forall es.
    ShuntPipelineEffects es =>
    OpLookup es ->
    RenamedDeclarationBody ->
    Eff es ShuntedDeclarationBody
shuntDeclarationBody opL (DeclarationBody rdb) = DeclarationBody <$> traverseOf unlocated go rdb
  where
    go :: RenamedDeclarationBody' -> Eff es ShuntedDeclarationBody'
    go (Value name e _ ty ann) = do
        shunted <- let ?lookup = opL in fixExpr e
        let ty' = fmap coerceType ty
        ann <- traverseValueDeclAnnotations (let ?lookup = opL in shuntAnnotation) ann
        pure (Value name shunted NoFieldValue ty' ann)
    go (TypeDeclaration name vars ty ann) = do
        annotations <- let ?lookup = opL in shuntTypeDeclAnnotations ann
        pure (TypeDeclaration name vars (coerceTypeDeclaration <$> ty) annotations)

shuntTypeDeclAnnotations ::
    (ShuntPipelineEffects r, HasOpLookup r) =>
    TypeDeclAnnotations Renamed ->
    Eff r (TypeDeclAnnotations Shunted)
shuntTypeDeclAnnotations (TypeDeclAnnotations k a) = TypeDeclAnnotations k <$> traverse shuntAnnotation a

-- | Shunt a renamed annotation using the given operator lookup function
shuntAnnotation :: (ShuntPipelineEffects r, HasOpLookup r) => Annotation Renamed -> Eff r (Annotation Shunted)
shuntAnnotation (Annotation name args) = do
    args' <- traverseOf (each % _Unwrapped) fixExpr args
    pure $ Annotation name args'

{- | Fix the operators in an expression to the correct precedence and shunt it
The main entry point for this module that simply combines 'fixOperators' and 'shuntExpr'
-}
fixExpr :: (ShuntPipelineEffects r, HasOpLookup r) => RenamedExpr -> Eff r ShuntedExpr
fixExpr e = do
    fixed <- fixOperators e
    shuntExpr fixed

{- | Shunt a renamed expression into a shunted expression.
This doesn't actually do much other than traverse the AST and convert types.
However, it does also convert binary operators into function calls.
-}
shuntExpr ::
    forall r.
    (ShuntPipelineEffects r, HasOpLookup r) =>
    RenamedExpr ->
    Eff r ShuntedExpr
shuntExpr (Expr (le, t)) = (\x -> Expr (x, coerceType <$> t)) <$> traverseOf unlocated shuntExpr' le
  where
    shuntExpr' :: RenamedExpr' -> Eff r ShuntedExpr'
    shuntExpr' (Int l) = pure (Int l)
    shuntExpr' (Float l) = pure (Float l)
    shuntExpr' (String l) = pure (String l)
    shuntExpr' (Char l) = pure (Char l)
    shuntExpr' Unit = pure Unit
    shuntExpr' (Var v) = pure (Var v)
    shuntExpr' (Constructor v) = pure (Constructor v)
    shuntExpr' (Lambda n e) = do
        e' <- fixExpr e
        n' <- traverseOf unlocated (\(TypedLambdaParam (v, t)) -> pure $ TypedLambdaParam (v, coerceType <$> t)) n
        pure (Lambda n' e')
    shuntExpr' (FunctionCall f x) = FunctionCall <$> fixExpr f <*> fixExpr x
    shuntExpr' (TypeApplication e t) = TypeApplication <$> fixExpr e <*> pure (coerceType t)
    shuntExpr' (BinaryOperator (operator, l, r)) = do
        -- turn the binary operator into 2 function calls
        -- (a `op` b) -> (op a) b
        -- ((a `op` b) `op` c) -> (op (op a b)) c
        -- Semantics for type annotation shifting are as follows:
        -- (a : T) `op` b -> (op (a : T)) b
        -- a `op` (b : T) -> (op a) (b : T)
        -- (a : T1) `op` (b : T2) -> (op (a : T1)) (b : T2)

        l' <- fixExpr l
        r' <- fixExpr r
        let op' = case operator ^. _Unwrapped % unlocated of
                SymOp lopName -> Var (OperatorVarName <<$>> lopName) `withLocationOf` lopName
                Infixed inName -> do
                    let z = case inName of
                            Global (Located l' (Qualified (VarName n) m)) ->
                                Var
                                    (Located l' (Global (Located l' (Qualified (NormalVarName n) m))))
                            Global (Located l' (Qualified (ConName n) m)) ->
                                Constructor
                                    (Located l' (Qualified n m))
                            Local (Located l' (Unique (VarName n) i)) ->
                                Var
                                    (Located l' (Local (Located l' (Unique (NormalVarName n) i))))
                            Local (Located _ (Unique (ConName _1) _)) -> error "Shouldn't have local con names"
                    z `withLocationOf` (operator ^. _Unwrapped)

        let opVar = Expr (op', Nothing) -- There can't ever be a type annotation on an operator
        let leftCall =
                Expr
                    ( Located
                        (Located.spanningRegion' (op' ^. sourceRegion :| [l' ^. _Unwrapped % _1 % sourceRegion]))
                        (FunctionCall opVar l')
                    , Nothing -- this will / can never have a type annotation
                    )
        pure (FunctionCall leftCall r')
    shuntExpr' (If cond then' else') = If <$> fixExpr cond <*> fixExpr then' <*> fixExpr else'
    shuntExpr' (Let vn _ e) = Let vn NoFieldValue <$> fixExpr e
    shuntExpr' (LetIn vn _ e body) = LetIn vn NoFieldValue <$> fixExpr e <*> fixExpr body
    shuntExpr' (Block e) = Block <$> traverse fixExpr e
    shuntExpr' (Match e cases) = do
        e' <- fixExpr e
        cases' <- traverse (bitraverse shuntPattern fixExpr) cases
        pure $ Match e' cases'
    shuntExpr' (InParens e) = (^. _Unwrapped % _1 % unlocated) <$> fixExpr e

shuntPattern :: RenamedPattern -> Eff r ShuntedPattern
shuntPattern (Pattern (le, t)) = (\x -> Pattern (x, coerceType <$> t)) <$> traverseOf unlocated shuntPattern' le
  where
    shuntPattern' :: RenamedPattern' -> Eff r ShuntedPattern'
    shuntPattern' (VarPattern v) = pure (VarPattern v)
    shuntPattern' (ConstructorPattern v p) = ConstructorPattern v <$> traverse shuntPattern p
    shuntPattern' WildcardPattern = pure WildcardPattern
    shuntPattern' UnitPattern = pure UnitPattern
    shuntPattern' (IntegerPattern l) = pure (IntegerPattern l)
    shuntPattern' (FloatPattern l) = pure (FloatPattern l)
    shuntPattern' (StringPattern l) = pure (StringPattern l)
    shuntPattern' (CharPattern l) = pure (CharPattern l)
