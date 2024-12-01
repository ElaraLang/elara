{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elara.Shunt where

import Data.Generics.Product (HasField' (field'))
import Data.Generics.Wrapped
import Data.Map qualified as Map
import Elara.AST.Generic
import Elara.AST.Generic.Common
import Elara.AST.Module
import Elara.AST.Name (Name (..), Qualified (..), VarName (..), VarOrConName (..))
import Elara.AST.Region (IgnoreLocation (..), Located (..), SourceRegion, sourceRegion, sourceRegionToDiagnosePosition, unlocated, withLocationOf)
import Elara.AST.Region qualified as Located
import Elara.AST.Renamed
import Elara.AST.Select
import Elara.AST.Shunted
import Elara.AST.VarRef
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style
import Elara.Data.TopologicalGraph
import Elara.Data.Unique (Unique (Unique))
import Elara.Error (ReportableError (..), runErrorOrReport)
import Elara.Error.Codes qualified as Codes
import Elara.Error.Effect (writeReport)
import Elara.Pipeline (EffectsAsPrefixOf, IsPipeline)
import Error.Diagnose
import Polysemy (Member, Members, Sem, subsume_)
import Polysemy.Error (Error, throw)
import Polysemy.Reader hiding (Local)
import Polysemy.State (State, execState, modify)
import Polysemy.Writer hiding (pass)
import Prelude hiding (modify')

type OpTable = Map (IgnoreLocVarRef Name) OpInfo

newtype Precedence = Precedence Int
    deriving (Show, Eq, Ord)

instance Pretty Precedence where
    pretty (Precedence i) = pretty i

mkPrecedence :: Int -> Precedence
mkPrecedence i
    | i < 0 = error "Precedence must be positive"
    | i > 9 = error "Precedence must be less than 10"
    | otherwise = Precedence i

data OpInfo = OpInfo
    { precedence :: !Precedence
    , associativity :: !Associativity
    }
    deriving (Show, Eq, Ord)

instance Pretty OpInfo where
    pretty (OpInfo p LeftAssociative) = Style.keyword "infixl" <+> pretty p
    pretty (OpInfo p RightAssociative) = Style.keyword "infixr" <+> pretty p
    pretty (OpInfo p NonAssociative) = Style.keyword "infix" <+> pretty p

data Associativity
    = LeftAssociative
    | RightAssociative
    | NonAssociative
    deriving (Show, Eq, Ord)

data ShuntError
    = SamePrecedenceError !(RenamedBinaryOperator, OpInfo) !(RenamedBinaryOperator, OpInfo)
    deriving (Show)

instance Exception ShuntError

prettyOp :: RenamedBinaryOperator -> Doc AnsiStyle
prettyOp (MkBinaryOperator op') = Style.operator $ case op' ^. unlocated of
    SymOp opName -> pretty (opName ^. unlocated)
    Infixed vn -> "`" <> pretty vn <> "`"

prettyOpTable :: OpTable -> Doc AnsiStyle
prettyOpTable table = brackets (vsep (map prettyOpInfo (Map.toList table)))
  where
    prettyOpInfo :: (IgnoreLocVarRef Name, OpInfo) -> Doc AnsiStyle
    prettyOpInfo (opName, info) =
        pretty info <+> pretty opName

instance ReportableError ShuntError where
    report (SamePrecedenceError (op1@(MkBinaryOperator op1'), a1) (op2@(MkBinaryOperator op2'), a2)) = do
        let op1Src = sourceRegionToDiagnosePosition $ op1' ^. sourceRegion
        let op2Src = sourceRegionToDiagnosePosition $ op2' ^. sourceRegion
        writeReport $
            Err
                (Just Codes.samePrecedence)
                ("Cannot mix operators with same precedence " <> prettyOp op1 <> " and " <> prettyOp op2 <> " when both operators have different associativity.")
                [(op1Src, This (pretty a1)), (op2Src, This (pretty a2))]
                [Hint "Add parentheses to resolve the ambiguity", Hint "Change the precedence of one of the operators", Hint "Change the associativity of one of the operators"]

data ShuntWarning
    = UnknownPrecedence OpTable RenamedBinaryOperator
    deriving (Eq, Ord, Show)

instance ReportableError ShuntWarning where
    report (UnknownPrecedence opTable lop@(MkBinaryOperator lOperator)) = do
        let opSrc = sourceRegionToDiagnosePosition $ lOperator ^. sourceRegion
        writeReport $
            Warn
                (Just Codes.unknownPrecedence)
                ( vsep
                    [ "Unknown precedence/associativity for operator" <+> prettyOp lop
                        <> ". The system will assume it has the highest precedence (9) and left associativity, but you should specify it manually. "
                    , "Known Operators:" <+> prettyOpTable opTable
                    ]
                )
                [(opSrc, This "operator")]
                [Hint "Define the precedence and associativity of the operator explicitly. There is currently no way of doing this lol"]

opInfo :: OpTable -> RenamedBinaryOperator -> Maybe OpInfo
opInfo table operator = case operator ^. _Unwrapped % unlocated of
    SymOp opName -> Map.lookup (ignoreLocation (NVarName . OperatorVarName <$> opName ^. unlocated)) table
    Infixed vn -> Map.lookup (ignoreLocation (toName <$> vn)) table
      where
        toName (VarName n) = NVarName (NormalVarName n)
        toName (ConName n) = NTypeName n

pattern InExpr :: RenamedExpr' -> RenamedExpr
pattern InExpr y <- Expr (Located _ y, _)

pattern InExpr' :: SourceRegion -> RenamedExpr' -> RenamedExpr
pattern InExpr' loc y <- Expr (Located loc y, _)

{-
 | Fix the operators in an expression to the correct precedence
 | For example given ((+) = 1l) and ((*) = 2r)
 | 1 + 2 * 3 * 4 + 5 + 6 should be parsed as (((1 + (2 * 3)) * 4) + 5) + 6
 | https://stackoverflow.com/a/67992584/6272977 This answer was a huge help in designing this
-}
fixOperators :: forall r. Members ShuntPipelineEffects r => OpTable -> RenamedExpr -> Sem r RenamedExpr
fixOperators opTable o = do
    reassoc o
  where
    withLocationOf' :: RenamedExpr -> RenamedExpr' -> RenamedExpr
    withLocationOf' s repl = over (_Unwrapped % _1) (repl <$) s

    reassoc :: RenamedExpr -> Sem r RenamedExpr
    reassoc (Expr (Located l (InParens e), t)) = do
        e' <- reassoc e
        pure (Expr (Located l (InParens e'), t))
    reassoc e@(InExpr' loc (BinaryOperator (operator, l, r))) = do
        l' <- fixOperators opTable l
        r' <- fixOperators opTable r
        withLocationOf' e <$> reassoc' loc operator l' r'
    reassoc e = pure e

    reassoc' :: SourceRegion -> RenamedBinaryOperator -> RenamedExpr -> RenamedExpr -> Sem r RenamedExpr'
    reassoc' sr o1 e1 r@(InExpr (BinaryOperator (o2, e2, e3))) = do
        info1 <- getInfoOrWarn o1
        info2 <- getInfoOrWarn o2
        case compare info1.precedence info2.precedence of
            GT -> assocLeft
            LT -> assocRight
            EQ -> case (info1.associativity, info2.associativity) of
                (LeftAssociative, LeftAssociative) -> assocLeft
                (RightAssociative, RightAssociative) -> assocRight
                (_, _) -> throw (SamePrecedenceError (o1, info1) (o2, info2))
      where
        assocLeft = do
            reassociated' <- reassoc' sr o1 e1 e2
            let reassociated = Expr (Located sr reassociated', Nothing)
            pure (BinaryOperator (o2, reassociated, e3))

        assocRight = pure (BinaryOperator (o1, e1, r))

        getInfoOrWarn :: RenamedBinaryOperator -> Sem r OpInfo
        getInfoOrWarn operator = case opInfo opTable operator of
            Just info -> pure info
            Nothing -> do
                tell (fromList [UnknownPrecedence opTable operator])
                pure (OpInfo (mkPrecedence 9) LeftAssociative)
    reassoc' _ operator l r = pure (BinaryOperator (operator, l, r))

type ShuntPipelineEffects = '[Error ShuntError, Writer (Set ShuntWarning)]
type InnerShuntPipelineEffects = '[Error ShuntError, Writer (Set ShuntWarning), Reader OpTable]

runShuntPipeline :: IsPipeline r => Sem (EffectsAsPrefixOf ShuntPipelineEffects r) a -> Sem r a
runShuntPipeline s =
    do
        (warnings, a) <-
            runWriter
                . runErrorOrReport
                . subsume_
                $ s
        traverse_ report warnings
        pure a

createOpTable :: IsPipeline r => Maybe OpTable -> TopologicalGraph (Module 'Renamed) -> Sem r OpTable
createOpTable prevOpTable graph = execState (maybeToMonoid prevOpTable) $ do
    traverseGraph_ addDeclsToOpTable graph
  where
    addDeclsToOpTable :: Member (State OpTable) r => Module 'Renamed -> Sem r ()
    addDeclsToOpTable = traverseModule_ addDeclsToOpTable'

    addDeclsToOpTable' :: Member (State OpTable) r => Declaration Renamed -> Sem r ()
    addDeclsToOpTable' (Declaration (Located _ decl)) = do
        case decl ^. field' @"body" % _Unwrapped % unlocated of
            Value{_valueName, _valueAnnotations = (ValueDeclAnnotations (Just fixity))} ->
                let nameRef :: IgnoreLocVarRef Name = Global $ IgnoreLocation $ (NVarName <<$>> _valueName)
                 in modify $ Map.insert nameRef (infixDeclToOpInfo fixity)
            TypeDeclaration name _ _ (TypeDeclAnnotations (Just fixity) NoFieldValue) ->
                let nameRef :: IgnoreLocVarRef Name = Global $ IgnoreLocation $ (sequenceA $ (Qualified (NTypeName <$> name) (decl ^. field' @"moduleName" % unlocated)))
                 in modify $ Map.insert nameRef (infixDeclToOpInfo fixity)
            _ -> pass

infixDeclToOpInfo :: InfixDeclaration Renamed -> OpInfo
infixDeclToOpInfo (InfixDeclaration name prec assoc) = OpInfo (Precedence $ prec ^. unlocated) (convAssoc $ assoc ^. unlocated)
  where
    convAssoc LeftAssoc = LeftAssociative
    convAssoc RightAssoc = RightAssociative
    convAssoc NonAssoc = NonAssociative

shuntGraph ::
    forall r.
    IsPipeline r =>
    Members ShuntPipelineEffects r =>
    Maybe OpTable ->
    TopologicalGraph (Module Renamed) ->
    Sem r (TopologicalGraph (Module Shunted))
shuntGraph prevOpTable graph = do
    opTable <- createOpTable prevOpTable graph
    runReader opTable $ traverseGraph shunt graph

shunt ::
    forall r.
    Members InnerShuntPipelineEffects r =>
    Module 'Renamed ->
    Sem r (Module 'Shunted)
shunt = traverseModule shuntDeclaration

shuntDeclaration ::
    forall r.
    Members InnerShuntPipelineEffects r =>
    RenamedDeclaration ->
    Sem r ShuntedDeclaration
shuntDeclaration (Declaration decl) =
    Declaration
        <$> traverseOf
            unlocated
            ( \(decl' :: RenamedDeclaration') -> do
                body' <- shuntDeclarationBody (decl' ^. field' @"body")
                pure (Declaration' (decl' ^. field' @"moduleName") body')
            )
            decl

shuntDeclarationBody ::
    forall r.
    Members InnerShuntPipelineEffects r =>
    RenamedDeclarationBody ->
    Sem r ShuntedDeclarationBody
shuntDeclarationBody (DeclarationBody rdb) = DeclarationBody <$> traverseOf unlocated shuntDeclarationBody' rdb
  where
    shuntDeclarationBody' :: RenamedDeclarationBody' -> Sem r ShuntedDeclarationBody'
    shuntDeclarationBody' (Value name e _ ty ann) = do
        shunted <- fixExpr e
        let ty' = fmap coerceType ty
        pure (Value name shunted NoFieldValue ty' (coerceValueDeclAnnotations ann))
    shuntDeclarationBody' (TypeDeclaration name vars ty ann) = pure (TypeDeclaration name vars (coerceTypeDeclaration <$> ty) (coerceTypeDeclAnnotations ann))

fixExpr :: Members InnerShuntPipelineEffects r => RenamedExpr -> Sem r ShuntedExpr
fixExpr e = do
    opTable <- ask
    fixed <- fixOperators opTable e
    shuntExpr fixed

shuntExpr ::
    forall r.
    Members InnerShuntPipelineEffects r =>
    RenamedExpr ->
    Sem r ShuntedExpr
shuntExpr (Expr (le, t)) = do
    (\x -> Expr (x, coerceType <$> t)) <$> traverseOf unlocated shuntExpr' le
  where
    shuntExpr' :: RenamedExpr' -> Sem r ShuntedExpr'
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
        -- error (showPretty (operator,l,r))
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
shuntPattern :: RenamedPattern -> Sem r ShuntedPattern
shuntPattern (Pattern (le, t)) = (\x -> Pattern (x, coerceType <$> t)) <$> traverseOf unlocated shuntPattern' le
  where
    shuntPattern' :: RenamedPattern' -> Sem r ShuntedPattern'
    shuntPattern' (VarPattern v) = pure (VarPattern v)
    shuntPattern' (ConstructorPattern v p) = ConstructorPattern v <$> traverse shuntPattern p
    shuntPattern' WildcardPattern = pure WildcardPattern
    shuntPattern' UnitPattern = pure UnitPattern
    shuntPattern' (IntegerPattern l) = pure (IntegerPattern l)
    shuntPattern' (FloatPattern l) = pure (FloatPattern l)
    shuntPattern' (StringPattern l) = pure (StringPattern l)
    shuntPattern' (CharPattern l) = pure (CharPattern l)
