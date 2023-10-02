{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elara.Shunt where

import Control.Lens (over, to, traverseOf, view, (^.), _1)
import Data.Generics.Product (HasField' (field'))
import Data.Generics.Wrapped
import Data.Map (lookup)
import Elara.AST.Generic
import Elara.AST.Generic.Common
import Elara.AST.Module
import Elara.AST.Name (Name (..), NameLike (fullNameText, nameText), Qualified (..), VarName (..), VarOrConName (..))
import Elara.AST.Region (Located (..), SourceRegion, sourceRegion, sourceRegionToDiagnosePosition, unlocated, withLocationOf)
import Elara.AST.Region qualified as Located
import Elara.AST.Renamed
import Elara.AST.Select
import Elara.AST.Shunted
import Elara.AST.VarRef
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style
import Elara.Data.Unique (Unique (Unique))
import Elara.Error (ReportableError (..), runErrorOrReport)
import Elara.Error.Codes qualified as Codes
import Elara.Error.Effect (writeReport)
import Elara.Pipeline (EffectsAsPrefixOf, IsPipeline)
import Error.Diagnose
import Polysemy (Members, Sem)
import Polysemy.Error (Error, throw)
import Polysemy.Reader hiding (Local)
import Polysemy.Writer
import Prelude hiding (modify')

type OpTable = Map (VarRef Name) OpInfo

newtype Precedence = Precedence Int
    deriving (Show, Eq, Ord)

mkPrecedence :: Int -> Precedence
mkPrecedence i
    | i < 0 = error "Precedence must be positive"
    | i > 9 = error "Precedence must be less than 10"
    | otherwise = Precedence i

data OpInfo = OpInfo
    { precedence :: Precedence
    , associativity :: Associativity
    }
    deriving (Show)

instance Pretty OpInfo

data Associativity
    = LeftAssociative
    | RightAssociative
    | NonAssociative
    deriving (Show)

data ShuntError
    = SamePrecedenceError (RenamedBinaryOperator, Associativity) (RenamedBinaryOperator, Associativity)
    deriving (Show)

instance Exception ShuntError

prettyOp :: RenamedBinaryOperator -> Doc AnsiStyle
prettyOp (MkBinaryOperator op') = Style.operator $ case op' ^. unlocated of
    SymOp opName -> pretty (fullNameText (opName ^. unlocated . to varRefVal))
    Infixed vn -> "`" <> pretty (nameText (vn ^. to varRefVal)) <> "`"

instance ReportableError ShuntError where
    report (SamePrecedenceError (op1@(MkBinaryOperator op1'), a1) (op2@(MkBinaryOperator op2'), a2)) = do
        let op1Src = sourceRegionToDiagnosePosition $ op1' ^. sourceRegion
        let op2Src = sourceRegionToDiagnosePosition $ op2' ^. sourceRegion
        writeReport $
            Err
                (Just Codes.samePrecedence)
                ("Cannot mix operators with same precedence " <> prettyOp op1 <> " and " <> prettyOp op2 <> " when both operators have different associativity.")
                [(op1Src, This (show a1)), (op2Src, This (show a2))]
                [Hint "Add parentheses to resolve the ambiguity", Hint "Change the precedence of one of the operators", Hint "Change the associativity of one of the operators"]

newtype ShuntWarning
    = UnknownPrecedence RenamedBinaryOperator
    deriving (Eq, Ord, Show)

instance ReportableError ShuntWarning where
    report (UnknownPrecedence (MkBinaryOperator lOperator)) = do
        let opSrc = sourceRegionToDiagnosePosition $ lOperator ^. sourceRegion
        let operatorName o = case o of
                SymOp opName -> nameText $ varRefVal (opName ^. unlocated)
                Infixed vn -> "`" <> nameText (varRefVal vn) <> "`"
        writeReport $
            Warn
                (Just Codes.unknownPrecedence)
                ("Unknown precedence/associativity for operator" <+> pretty (operatorName (lOperator ^. unlocated)) <> ". The system will assume it has the highest precedence (9) and left associativity, but you should specify it manually. ")
                [(opSrc, This "operator")]
                [Hint "Define the precedence and associativity of the operator explicitly. There is currently no way of doing this lol"]

opInfo :: OpTable -> RenamedBinaryOperator -> Maybe OpInfo
opInfo table operator = case operator ^. _Unwrapped . unlocated of
    SymOp opName -> lookup (NOpName <$> opName ^. unlocated) table
    Infixed vn -> lookup (toName <$> vn) table
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
fixOperators :: forall r. (Members ShuntPipelineEffects r) => OpTable -> RenamedExpr -> Sem r RenamedExpr
fixOperators opTable = reassoc
  where
    withLocationOf' :: RenamedExpr -> RenamedExpr' -> RenamedExpr
    withLocationOf' s repl = over (_Unwrapped . _1) (repl <$) s

    reassoc :: RenamedExpr -> Sem r RenamedExpr
    reassoc e@(InExpr' loc (BinaryOperator (operator, l, r))) = do
        l' <- reassoc l
        r' <- reassoc r
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
                (a1, a2) -> throw (SamePrecedenceError (o1, a1) (o2, a2))
      where
        assocLeft = do
            reassociated' <- reassoc' sr o1 e1 e2
            let reassociated = Expr (Located sr reassociated', Nothing)
            pure (BinaryOperator (o2, reassociated, e3))

        assocRight = do
            pure (BinaryOperator (o1, e1, r))

        getInfoOrWarn :: RenamedBinaryOperator -> Sem r OpInfo
        getInfoOrWarn operator = case opInfo opTable operator of
            Just info -> pure info
            Nothing -> do
                tell (fromList [UnknownPrecedence operator])
                pure (OpInfo (mkPrecedence 9) LeftAssociative)
    reassoc' _ operator l r = pure (BinaryOperator (operator, l, r))

type ShuntPipelineEffects = '[Error ShuntError, Writer (Set ShuntWarning), Reader OpTable]

runShuntPipeline :: (IsPipeline r) => OpTable -> Sem (EffectsAsPrefixOf ShuntPipelineEffects r) a -> Sem r a
runShuntPipeline opTable s =
    do
        (warnings, a) <-
            runReader opTable
                . runWriter
                . runErrorOrReport
                $ s
        traverse_ report warnings
        pure a

shunt ::
    forall r.
    ( Members ShuntPipelineEffects r
    ) =>
    Module 'Renamed ->
    Sem r (Module 'Shunted)
shunt = traverseModule shuntDeclaration

shuntDeclaration ::
    forall r.
    ( Members ShuntPipelineEffects r
    ) =>
    RenamedDeclaration ->
    Sem r ShuntedDeclaration
shuntDeclaration (Declaration decl) =
    Declaration
        <$> traverseOf
            unlocated
            ( \(decl' :: RenamedDeclaration') -> do
                body' <- shuntDeclarationBody (decl' ^. field' @"body")
                pure (Declaration' (decl' ^. field' @"moduleName") (decl' ^. field' @"name") body')
            )
            decl

shuntDeclarationBody ::
    forall r.
    ( Members ShuntPipelineEffects r
    ) =>
    RenamedDeclarationBody ->
    Sem r ShuntedDeclarationBody
shuntDeclarationBody (DeclarationBody rdb) = DeclarationBody <$> traverseOf unlocated shuntDeclarationBody' rdb
  where
    shuntDeclarationBody' :: RenamedDeclarationBody' -> Sem r ShuntedDeclarationBody'
    shuntDeclarationBody' (Value e _ ty) = do
        shunted <- fixExpr e
        let ty' = fmap coerceType ty
        pure (Value shunted NoFieldValue ty')
    shuntDeclarationBody' (TypeDeclaration vars ty) = pure (TypeDeclaration vars (coerceTypeDeclaration <$> ty))

fixExpr :: (Members ShuntPipelineEffects r) => RenamedExpr -> Sem r ShuntedExpr
fixExpr e = do
    opTable <- ask
    fixed <- fixOperators opTable e
    shuntExpr fixed

shuntExpr ::
    forall r.
    ( Members ShuntPipelineEffects r
    ) =>
    RenamedExpr ->
    Sem r ShuntedExpr
shuntExpr (Expr (le, t)) = (\x -> Expr (x, coerceType <$> t)) <$> traverseOf unlocated shuntExpr' le
  where
    shuntExpr' :: RenamedExpr' -> Sem r ShuntedExpr'
    shuntExpr' (Int l) = pure (Int l)
    shuntExpr' (Float l) = pure (Float l)
    shuntExpr' (String l) = pure (String l)
    shuntExpr' (Char l) = pure (Char l)
    shuntExpr' Unit = pure Unit
    shuntExpr' (Var v) = pure (Var v)
    shuntExpr' (Constructor v) = pure (Constructor v)
    shuntExpr' (Lambda n e) = Lambda n <$> shuntExpr e
    shuntExpr' (FunctionCall f x) = FunctionCall <$> shuntExpr f <*> shuntExpr x
    shuntExpr' (TypeApplication e t) = TypeApplication <$> shuntExpr e <*> pure (coerceType t)
    shuntExpr' (BinaryOperator (operator, l, r)) = do
        -- turn the binary operator into 2 function calls
        -- (a `op` b) -> (op a) b
        -- Semantics for type annotation shifting are as follows:
        -- (a : T) `op` b -> (op (a : T)) b
        -- a `op` (b : T) -> (op a) (b : T)
        -- (a : T1) `op` (b : T2) -> (op (a : T1)) (b : T2)

        l' <- shuntExpr l
        r' <- shuntExpr r
        let op' = case operator ^. _Unwrapped . unlocated of
                SymOp lopName -> Var (OperatorVarName <<$>> lopName) `withLocationOf` lopName
                Infixed inName -> do
                    let z :: Expr' 'Shunted = case inName of
                            Global (Located l' (Qualified (VarName n) m)) ->
                                Var
                                    ( Located l' (Global (Located l' (Qualified (NormalVarName n) m)))
                                    )
                            Global (Located l' (Qualified (ConName n) m)) ->
                                Constructor
                                    ( Located l' (Qualified n m)
                                    )
                            Local (Located l' (Unique (VarName n) i)) ->
                                Var
                                    ( Located l' (Local (Located l' (Unique (NormalVarName n) i)))
                                    )
                            Local (Located _ (Unique (ConName _1) _)) -> error "Shouldn't have local con names"
                    z `withLocationOf` (operator ^. _Unwrapped)

        let opVar = Expr (op', Nothing) -- There can't ever be a type annotation on an operator
        let leftCall =
                Expr
                    ( Located
                        (Located.spanningRegion' (op' ^. sourceRegion :| [l' ^. _Unwrapped . _1 . sourceRegion]))
                        (FunctionCall opVar l')
                    , Nothing -- this will / can never have a type annotation
                    )
        pure (FunctionCall leftCall r')
    shuntExpr' (List es) = List <$> traverse shuntExpr es
    shuntExpr' (If cond then' else') = If <$> shuntExpr cond <*> shuntExpr then' <*> shuntExpr else'
    shuntExpr' (Let vn _ e) = Let vn NoFieldValue <$> shuntExpr e
    shuntExpr' (LetIn vn _ e body) = (LetIn vn NoFieldValue <$> shuntExpr e) <*> shuntExpr body
    shuntExpr' (Block e) = Block <$> traverse shuntExpr e
    shuntExpr' (Match e cases) = do
        e' <- shuntExpr e
        cases' <- traverse (bitraverse shuntPattern shuntExpr) cases
        pure $ Match e' cases'
    shuntExpr' (Tuple es) = Tuple <$> traverse shuntExpr es

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
    shuntPattern' (ListPattern ps) = ListPattern <$> traverse shuntPattern ps
    shuntPattern' (ConsPattern p1 p2) = ConsPattern <$> shuntPattern p1 <*> shuntPattern p2
