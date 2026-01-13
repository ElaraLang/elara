{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Elara.AST.Generic.Instances.StripLocation where

import Data.Kind (Constraint)
import Data.Kind qualified as Kind
import Elara.AST.Generic.Types
import Elara.AST.Generic.Utils
import Elara.AST.Region
import Elara.AST.Select
import Elara.AST.StripLocation
import GHC.Generics qualified as Generic

type TypeDeps :: (Kind.Type -> Kind.Type -> Constraint) -> LocatedAST -> UnlocatedAST -> Constraint
type TypeDeps cls ast1 ast2 =
    ( cls (Select TypeKind ast1) (Select TypeKind ast2)
    , cls (Select TupleType ast1) (Select TupleType ast2)
    , cls (CleanupLocated (Located (Select ASTTypeVar ast1))) (Select ASTTypeVar ast2)
    , cls (CleanupLocated (Located (Select UserDefinedType ast1))) (Select UserDefinedType ast2)
    )

type PatternDeps :: (Kind.Type -> Kind.Type -> Constraint) -> LocatedAST -> UnlocatedAST -> Constraint
type PatternDeps cls ast1 ast2 =
    ( cls (Select PatternType ast1) (Select PatternType ast2)
    , cls (CleanupLocated (Located (Select VarPat ast1))) (Select VarPat ast2)
    , cls (CleanupLocated (Located (Select ConPat ast1))) (Select ConPat ast2)
    , cls (Select ListPattern ast1) (Select ListPattern ast2)
    , cls (Select TuplePattern ast1) (Select TuplePattern ast2)
    , cls (Select ConsPattern ast1) (Select ConsPattern ast2)
    )

type ExprDeps :: (Kind.Type -> Kind.Type -> Constraint) -> LocatedAST -> UnlocatedAST -> Constraint
type ExprDeps cls ast1 ast2 =
    ( cls (Select (ASTType ForExpr) ast1) (Select (ASTType ForExpr) ast2)
    , cls (Select TypeApplication ast1) (Select TypeApplication ast2)
    , cls (Select ASTBinaryOperator ast1) (Select ASTBinaryOperator ast2)
    , cls (Select List ast1) (Select List ast2)
    , cls (Select LetPattern ast1) (Select LetPattern ast2)
    , cls (Select Tuple ast1) (Select Tuple ast2)
    , cls (Select InParens ast1) (Select InParens ast2)
    , cls (CleanupLocated (Located (Select ASTVarRef ast1))) (Select ASTVarRef ast2)
    , cls (CleanupLocated (Located (Select ConRef ast1))) (Select ConRef ast2)
    , cls (CleanupLocated (Located (Select LambdaPattern ast1))) (Select LambdaPattern ast2)
    , cls (CleanupLocated (Located (Select LetParamName ast1))) (Select LetParamName ast2)
    , PatternDeps cls ast1 ast2
    , TypeDeps cls ast1 ast2
    )

instance
    forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
    ( ASTLocate' ast1 ~ Located
    , ASTLocate' ast2 ~ Unlocated
    , ExprDeps StripLocation ast1 ast2
    ) =>
    StripLocation (Expr ast1) (Expr ast2)
    where
    stripLocation = stripExprLocation @ast1 @ast2

stripExprLocation ::
    forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
    ( ASTLocate' ast1 ~ Located
    , ASTLocate' ast2 ~ Unlocated
    , ExprDeps StripLocation ast1 ast2
    ) =>
    Expr ast1 ->
    Expr ast2
stripExprLocation (Expr (e :: ASTLocate ast1 (Expr' ast1), t)) =
    let e' :: ASTLocate ast1 (Expr' ast2) = fmapUnlocated @LocatedAST @ast1 stripExprLocation' e
     in Expr
            ( stripLocation e'
            , stripLocation t
            )
  where
    stripExprLocation' :: Expr' ast1 -> Expr' ast2
    stripExprLocation' = Generic.to . gStripLocation . Generic.from

instance
    forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
    ( ASTLocate' ast1 ~ Located
    , ASTLocate' ast2 ~ Unlocated
    , PatternDeps StripLocation ast1 ast2
    , TypeDeps StripLocation ast1 ast2
    ) =>
    StripLocation (Pattern ast1) (Pattern ast2)
    where
    stripLocation = stripPatternLocation @ast1 @ast2

stripPatternLocation ::
    forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
    ( ASTLocate' ast1 ~ Located
    , ASTLocate' ast2 ~ Unlocated
    , PatternDeps StripLocation ast1 ast2
    , TypeDeps StripLocation ast1 ast2
    ) =>
    Pattern ast1 ->
    Pattern ast2
stripPatternLocation (Pattern (p :: ASTLocate ast1 (Pattern' ast1), t)) =
    let p' :: ASTLocate ast1 (Pattern' ast2) = fmapUnlocated @LocatedAST @ast1 stripPatternLocation' p
     in Pattern
            ( stripLocation p'
            , stripLocation t
            )
  where
    stripPatternLocation' = Generic.to . gStripLocation . Generic.from

instance
    forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
    ( ASTLocate' ast1 ~ Located
    , ASTLocate' ast2 ~ Unlocated
    , (StripLocation (Select Infixed ast1) (Select Infixed ast2))
    , ( StripLocation
            (CleanupLocated (Located (Select SymOp ast1)))
            (Select SymOp ast2)
      )
    ) =>
    StripLocation (BinaryOperator ast1) (BinaryOperator ast2)
    where
    stripLocation = stripBinaryOperatorLocation @ast1 @ast2

stripBinaryOperatorLocation ::
    forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
    ( (ASTLocate' ast1 ~ Located)
    , ASTLocate' ast2 ~ Unlocated
    , StripLocation (ASTLocate ast1 (BinaryOperator' ast2)) (BinaryOperator' ast2)
    , _
    ) =>
    BinaryOperator ast1 ->
    BinaryOperator ast2
stripBinaryOperatorLocation (MkBinaryOperator (op :: ASTLocate ast1 (BinaryOperator' ast1))) =
    let op' = fmapUnlocated @LocatedAST @ast1 stripBinaryOperatorLocation' op
     in MkBinaryOperator (stripLocation op' :: BinaryOperator' ast2)
  where
    stripBinaryOperatorLocation' :: BinaryOperator' ast1 -> BinaryOperator' ast2
    stripBinaryOperatorLocation' (SymOp name) = SymOp (stripLocation name)
    stripBinaryOperatorLocation' (Infixed name) = Infixed (stripLocation name)

instance
    forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
    ( (ASTLocate' ast1 ~ Located)
    , ASTLocate' ast2 ~ Unlocated
    , TypeDeps StripLocation ast1 ast2
    ) =>
    StripLocation (Type ast1) (Type ast2)
    where
    stripLocation = stripTypeLocation @ast1 @ast2

stripTypeLocation ::
    forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
    ( ASTLocate' ast1 ~ Located
    , ASTLocate' ast2 ~ Unlocated
    , TypeDeps StripLocation ast1 ast2
    ) =>
    Type ast1 ->
    Type ast2
stripTypeLocation (Type (t :: ASTLocate ast1 (Type' ast1), kind)) =
    let t' :: ASTLocate ast1 (Type' ast2) = fmapUnlocated @LocatedAST @ast1 stripTypeLocation' t
     in Type (stripLocation t', stripLocation kind)
  where
    stripTypeLocation' = Generic.to . gStripLocation . Generic.from
