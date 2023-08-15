{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elara.Shunt where

import Control.Lens
import Data.Map (lookup)
import Elara.AST.Lenses (HasDeclarationBody (..))
import Elara.AST.Module
import Elara.AST.Name (Name (NOpName, NVarName), NameLike (fullNameText, nameText), VarName (OperatorVarName))
import Elara.AST.Region (Located (..), SourceRegion, sourceRegion, sourceRegionToDiagnosePosition, unlocated, withLocationOf)
import Elara.AST.Region qualified as Located
import Elara.AST.Renamed (BinaryOperator (MkBinaryOperator))
import Elara.AST.Renamed qualified as Renamed
import Elara.AST.Select
import Elara.AST.Shunted qualified as Shunted
import Elara.AST.VarRef
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style
import Elara.Error (ReportableError (..))
import Elara.Error.Codes qualified as Codes
import Elara.Error.Effect (writeReport)
import Error.Diagnose
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)
import Polysemy.Reader
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
    deriving (Show, Eq)

data Associativity
    = LeftAssociative
    | RightAssociative
    | NonAssociative
    deriving (Show, Eq)

data ShuntError
    = SamePrecedenceError (Renamed.BinaryOperator, Associativity) (Renamed.BinaryOperator, Associativity)
    deriving (Show, Eq)

prettyOp :: Renamed.BinaryOperator -> Doc AnsiStyle
prettyOp (Renamed.MkBinaryOperator op') = Style.operator $ case op' ^. unlocated of
    Renamed.Op opName -> pretty (fullNameText (opName ^. unlocated . to varRefVal))
    Renamed.Infixed vn -> "`" <> pretty (nameText (vn ^. unlocated . to varRefVal)) <> "`"

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
    = UnknownPrecedence Renamed.BinaryOperator
    deriving (Show, Eq, Ord)

instance ReportableError ShuntWarning where
    report (UnknownPrecedence (Renamed.MkBinaryOperator lOperator)) = do
        let opSrc = sourceRegionToDiagnosePosition $ lOperator ^. sourceRegion
        let operatorName o = case o of
                Renamed.Op opName -> nameText $ varRefVal (opName ^. unlocated)
                Renamed.Infixed vn -> "`" <> nameText (varRefVal (vn ^. unlocated)) <> "`"
        writeReport $
            Warn
                (Just Codes.unknownPrecedence)
                ("Unknown precedence/associativity for operator" <+> pretty (operatorName (lOperator ^. unlocated)) <> ". The system will assume it has the highest precedence (9) and left associativity, but you should specify it manually. ")
                [(opSrc, This "operator")]
                [Hint "Define the precedence and associativity of the operator explicitly"]

opInfo :: OpTable -> Renamed.BinaryOperator -> Maybe OpInfo
opInfo table operator = case operator ^. Renamed._MkBinaryOperator . unlocated of
    Renamed.Op opName -> lookup (NOpName <$> opName ^. unlocated) table
    Renamed.Infixed vn -> lookup (NVarName <$> vn ^. unlocated) table

pattern InExpr :: Renamed.Expr' -> Renamed.Expr
pattern InExpr y <- Renamed.Expr (Located _ y)

pattern InExpr' :: SourceRegion -> Renamed.Expr' -> Renamed.Expr
pattern InExpr' loc y <- Renamed.Expr (Located loc y)

{-
 | Fix the operators in an expression to the correct precedence
 | For example given ((+) = 1l) and ((*) = 2r)
 | 1 + 2 * 3 * 4 + 5 + 6 should be parsed as (((1 + (2 * 3)) * 4) + 5) + 6
 | https://stackoverflow.com/a/67992584/6272977 This answer was a huge help in designing this
-}
fixOperators :: forall r. (Member (Error ShuntError) r, Member (Writer (Set ShuntWarning)) r) => OpTable -> Renamed.Expr -> Sem r Renamed.Expr
fixOperators opTable = reassoc
  where
    withLocationOf' :: Renamed.Expr -> Renamed.Expr' -> Renamed.Expr
    withLocationOf' s repl = over Renamed._Expr (repl <$) s

    reassoc :: Renamed.Expr -> Sem r Renamed.Expr
    reassoc e@(InExpr (Renamed.InParens e2)) = withLocationOf' e . Renamed.InParens <$> reassoc e2
    reassoc e@(InExpr' loc (Renamed.BinaryOperator operator l r)) = do
        l' <- reassoc l
        r' <- reassoc r
        withLocationOf' e <$> reassoc' loc operator l' r'
    reassoc e = pure e

    reassoc' :: SourceRegion -> Renamed.BinaryOperator -> Renamed.Expr -> Renamed.Expr -> Sem r Renamed.Expr'
    reassoc' sr operator l (InExpr (Renamed.InParens r)) = reassoc' sr operator l r
    reassoc' sr o1 e1 r@(InExpr (Renamed.BinaryOperator o2 e2 e3)) = do
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
            reassociated <- Renamed.Expr . Located sr <$> reassoc' sr o1 e1 e2
            pure (Renamed.BinaryOperator o2 (withLocationOf' reassociated (Renamed.InParens reassociated)) e3)

        assocRight = do
            pure (Renamed.BinaryOperator o1 e1 r)

        getInfoOrWarn :: Renamed.BinaryOperator -> Sem r OpInfo
        getInfoOrWarn operator = case opInfo opTable operator of
            Just info -> pure info
            Nothing -> do
                tell (fromList [UnknownPrecedence operator])
                pure (OpInfo (mkPrecedence 9) LeftAssociative)
    reassoc' _ operator l r = pure (Renamed.BinaryOperator operator l r)

shunt ::
    forall r.
    ( Member (Error ShuntError) r
    , Member (Writer (Set ShuntWarning)) r
    , Member (Reader OpTable) r
    ) =>
    Module Renamed ->
    Sem r (Module Shunted)
shunt = traverseModule shuntDeclaration

shuntDeclaration ::
    forall r.
    ( Member (Error ShuntError) r
    , Member (Writer (Set ShuntWarning)) r
    , Member (Reader OpTable) r
    ) =>
    Renamed.Declaration ->
    Sem r Shunted.Declaration
shuntDeclaration (Renamed.Declaration decl) =
    Shunted.Declaration
        <$> traverseOf
            unlocated
            ( \(decl' :: Renamed.Declaration') -> do
                body' <- traverseOf unlocated shuntDeclarationBody (decl' ^. declarationBody)
                pure (Shunted.Declaration' (decl' ^. moduleName) (decl' ^. name) body')
            )
            decl

shuntDeclarationBody ::
    forall r.
    ( Member (Error ShuntError) r
    , Member (Writer (Set ShuntWarning)) r
    , Member (Reader OpTable) r
    ) =>
    Renamed.DeclarationBody ->
    Sem r Shunted.DeclarationBody
shuntDeclarationBody (Renamed.DeclarationBody rdb) = Shunted.DeclarationBody <$> traverseOf unlocated shuntDeclarationBody' rdb
  where
    shuntDeclarationBody' :: Renamed.DeclarationBody' -> Sem r Shunted.DeclarationBody'
    shuntDeclarationBody' (Renamed.Value e ty) = do
        opTable <- ask
        fixed <- fixOperators opTable e
        shunted <- shuntExpr fixed
        pure (Shunted.Value shunted ty)
    shuntDeclarationBody' (Renamed.TypeDeclaration vars ty) = pure (Shunted.TypeDeclaration vars ty)

shuntExpr ::
    forall r.
    ( Member (Error ShuntError) r
    , Member (Writer (Set ShuntWarning)) r
    , Member (Reader OpTable) r
    ) =>
    Renamed.Expr ->
    Sem r Shunted.Expr
shuntExpr (Renamed.Expr le) = Shunted.Expr <$> traverseOf unlocated shuntExpr' le
  where
    shuntExpr' :: Renamed.Expr' -> Sem r Shunted.Expr'
    shuntExpr' (Renamed.Int l) = pure (Shunted.Int l)
    shuntExpr' (Renamed.Float l) = pure (Shunted.Float l)
    shuntExpr' (Renamed.String l) = pure (Shunted.String l)
    shuntExpr' (Renamed.Char l) = pure (Shunted.Char l)
    shuntExpr' Renamed.Unit = pure Shunted.Unit
    shuntExpr' (Renamed.Var v) = pure (Shunted.Var v)
    shuntExpr' (Renamed.Constructor v) = pure (Shunted.Constructor v)
    shuntExpr' (Renamed.Lambda n e) = Shunted.Lambda n <$> shuntExpr e
    shuntExpr' (Renamed.FunctionCall f x) = Shunted.FunctionCall <$> shuntExpr f <*> shuntExpr x
    shuntExpr' (Renamed.BinaryOperator operator l r) = do
        -- turn the binary operator into a function call
        -- (a `op` b) -> op a b
        l' <- shuntExpr l
        r' <- shuntExpr r
        let op' = case operator ^. Renamed._MkBinaryOperator . unlocated of
                Renamed.Op lopName -> OperatorVarName <<$>> lopName
                Renamed.Infixed inName -> inName
        let opVar = Shunted.Expr (Shunted.Var op' `withLocationOf` op')
        let leftCall =
                Shunted.Expr
                    ( Located
                        (Located.spanningRegion' (op' ^. sourceRegion :| [l' ^. Shunted._Expr . sourceRegion]))
                        (Shunted.FunctionCall opVar l')
                    )
        pure (Shunted.FunctionCall leftCall r')
    shuntExpr' (Renamed.List es) = Shunted.List <$> traverse shuntExpr es
    shuntExpr' (Renamed.If cond then' else') = Shunted.If <$> shuntExpr cond <*> shuntExpr then' <*> shuntExpr else'
    shuntExpr' (Renamed.Let vn e) = Shunted.Let vn <$> shuntExpr e
    shuntExpr' (Renamed.LetIn vn e body) = (Shunted.LetIn vn <$> shuntExpr e) <*> shuntExpr body
    shuntExpr' (Renamed.InParens e) = view unlocated <$> traverse shuntExpr' (e ^. Renamed._Expr)
    shuntExpr' (Renamed.Block e) = Shunted.Block <$> traverse shuntExpr e
    shuntExpr' (Renamed.Match e cases) = do
        e' <- shuntExpr e
        cases' <- traverse (bitraverse shuntPattern shuntExpr) cases
        pure $ Shunted.Match e' cases'
    shuntExpr' (Renamed.Tuple es) = Shunted.Tuple <$> traverse shuntExpr es

shuntPattern :: Renamed.Pattern -> Sem r Shunted.Pattern
shuntPattern (Renamed.Pattern lp) = Shunted.Pattern <$> traverseOf unlocated shuntPattern' lp
  where
    shuntPattern' :: Renamed.Pattern' -> Sem r Shunted.Pattern'
    shuntPattern' (Renamed.VarPattern v) = pure (Shunted.VarPattern v)
    shuntPattern' (Renamed.ConstructorPattern v p) = Shunted.ConstructorPattern v <$> traverse shuntPattern p
    shuntPattern' Renamed.WildcardPattern = pure Shunted.WildcardPattern
    shuntPattern' (Renamed.IntegerPattern l) = pure (Shunted.IntegerPattern l)
    shuntPattern' (Renamed.FloatPattern l) = pure (Shunted.FloatPattern l)
    shuntPattern' (Renamed.StringPattern l) = pure (Shunted.StringPattern l)
    shuntPattern' (Renamed.CharPattern l) = pure (Shunted.CharPattern l)
    shuntPattern' (Renamed.ListPattern ps) = Shunted.ListPattern <$> traverse shuntPattern ps
    shuntPattern' (Renamed.ConsPattern p1 p2) = Shunted.ConsPattern <$> shuntPattern p1 <*> shuntPattern p2
