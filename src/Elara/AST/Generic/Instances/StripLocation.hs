{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Elara.AST.Generic.Instances.StripLocation where

import Elara.AST.Generic.Types
import Elara.AST.Generic.Utils
import Elara.AST.Region
import Elara.AST.Select
import Elara.AST.StripLocation
import Elara.Data.AtLeast2List
import Relude.Extra (bimapF)

instance
    forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
    ( ASTLocate' ast1 ~ Located
    , ASTLocate' ast2 ~ Unlocated
    , (StripLocation (Select LambdaPattern ast1) (Select LambdaPattern ast2))
    , (StripLocation (Select LetPattern ast1) (Select LetPattern ast2))
    , StripLocation (Select TypeApplication ast1) (Select TypeApplication ast2)
    , (ApplyAsFunctorish (Select (ASTType ForExpr) ast1) (Select (ASTType ForExpr) ast2) (Type ast1) (Type ast2))
    , (ApplyAsFunctorish (Select PatternType ast1) (Select PatternType ast2) (Type ast1) (Type ast2))
    , ApplyAsFunctorish (Select ConsPattern ast1) (Select ConsPattern ast2) (Pattern ast1) (Pattern ast2)
    , ApplyAsFunctorish (Select ListPattern ast1) (Select ListPattern ast2) (Pattern ast1) (Pattern ast2)
    , (StripLocation (Select Infixed ast1) (Select Infixed ast2))
    , (StripLocation (CleanupLocated (Located (Select SymOp ast1))) (Select SymOp ast2))
    , (StripLocation (CleanupLocated (Located (Select ASTTypeVar ast1))) (Select ASTTypeVar ast2))
    , (StripLocation (CleanupLocated (Located (Select ASTVarRef ast1))) (Select ASTVarRef ast2))
    , (StripLocation (CleanupLocated (Located (Select VarPat ast1))) (Select VarPat ast2))
    , (StripLocation (CleanupLocated (Located (Select ConPat ast1))) (Select ConPat ast2))
    , (StripLocation (CleanupLocated (Located (Select ConRef ast1))) (Select ConRef ast2))
    , (StripLocation (CleanupLocated (Located (Select LambdaPattern ast1))) (Select LambdaPattern ast1))
    , (StripLocation (CleanupLocated (Located (Select LetParamName ast1))) (Select LetParamName ast2))
    , (StripLocation (CleanupLocated (Located (Select UserDefinedType ast1))) (Select UserDefinedType ast2))
    , (DataConAs (Select ASTBinaryOperator ast1) (BinaryOperator ast1, Expr ast1, Expr ast1))
    , (DataConAs (Select ASTBinaryOperator ast2) (BinaryOperator ast2, Expr ast2, Expr ast2))
    , (DataConAs (Select InParens ast1) (Expr ast1))
    , (DataConAs (Select List ast1) [Expr ast1])
    , (DataConAs (Select List ast2) [Expr ast2])
    , (DataConAs (Select InParens ast2) (Expr ast2))
    , StripLocation (Select TypeKind ast1) (Select TypeKind ast2)
    , DataConAs (Select Tuple ast1) (AtLeast2List (Expr ast1))
    , DataConAs (Select Tuple ast2) (AtLeast2List (Expr ast2))
    , ApplyAsFunctorish
        (Select TuplePattern ast1)
        (Select TuplePattern ast2)
        (Pattern ast1)
        (Pattern ast2)
    , StripLocation
        (Select TupleType ast1)
        (Select TupleType ast2)
    , StripLocation
        (CleanupLocated (Located (Select TupleType ast1)))
        (Select TupleType ast1)
    , DataConAs
        (Select Tuple ast1)
        (NonEmpty (Expr ast1))
    , DataConAs
        (Select TupleType ast1)
        (AtLeast2List (Type ast1))
    , DataConAs
        (Select Tuple ast2)
        (NonEmpty (Expr ast2))
    , DataConAs
        (Select TupleType ast2)
        (AtLeast2List (Type ast2))
    ) =>
    StripLocation (Expr ast1) (Expr ast2)
    where
    stripLocation = stripExprLocation @ast1 @ast2

stripExprLocation ::
    forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
    ( ASTLocate' ast1 ~ Located
    , ASTLocate' ast2 ~ Unlocated
    , StripLocation (Select LambdaPattern ast1) (Select LambdaPattern ast2)
    , StripLocation (Select TypeApplication ast1) (Select TypeApplication ast2)
    , StripLocation (Select LetPattern ast1) (Select LetPattern ast2)
    , ApplyAsFunctorish (Select (ASTType ForExpr) ast1) (Select (ASTType ForExpr) ast2) (Type ast1) (Type ast2)
    , DataConAs (Select ASTBinaryOperator ast1) (BinaryOperator ast1, Expr ast1, Expr ast1)
    , _
    ) =>
    Expr ast1 ->
    Expr ast2
stripExprLocation (Expr (e :: ASTLocate ast1 (Expr' ast1), t)) =
    let e' = fmapUnlocated @LocatedAST @ast1 stripExprLocation' e
     in Expr
            ( stripLocation e'
            , applyAsFunctorish @(Select (ASTType ForExpr) ast1) @(Select (ASTType ForExpr) ast2) @(Type ast1) @(Type ast2)
                stripTypeLocation
                t
            )
  where
    stripExprLocation' :: Expr' ast1 -> Expr' ast2
    stripExprLocation' (Int i) = Int i
    stripExprLocation' (Float f) = Float f
    stripExprLocation' (String s) = String s
    stripExprLocation' (Char c) = Char c
    stripExprLocation' Unit = Unit
    stripExprLocation' (Var v) = Var (stripLocation v)
    stripExprLocation' (Constructor c) = Constructor (stripLocation c)
    stripExprLocation' (Lambda ps e) =
        let ps' = stripLocation ps
            ps'' =
                stripLocation @(Select LambdaPattern ast1) @(Select LambdaPattern ast2)
                    ps'
         in Lambda ps'' (stripExprLocation e)
    stripExprLocation' (FunctionCall e1 e2) = FunctionCall (stripExprLocation e1) (stripExprLocation e2)
    stripExprLocation' (TypeApplication e1 t1) =
        let t1' = stripLocation t1
            t1'' = stripLocation @(Select TypeApplication ast1) @(Select TypeApplication ast2) t1'
         in TypeApplication
                (stripExprLocation e1)
                t1''
    stripExprLocation' (If e1 e2 e3) = If (stripExprLocation e1) (stripExprLocation e2) (stripExprLocation e3)
    stripExprLocation' (BinaryOperator b) =
        let (op, e1, e2) = dataConAs @(Select ASTBinaryOperator ast1) @(BinaryOperator ast1, Expr ast1, Expr ast1) b
         in BinaryOperator $ asDataCon (stripBinaryOperatorLocation @ast1 @ast2 op, stripExprLocation e1, stripExprLocation e2)
    stripExprLocation' (List l) =
        let l' = dataConAs @(Select List ast1) @[Expr ast1] l
         in List $ asDataCon (stripExprLocation <$> l')
    stripExprLocation' (Match e m) = Match (stripExprLocation e) (bimapF stripPatternLocation stripExprLocation m)
    stripExprLocation' (LetIn v p e1 e2) =
        let p' = stripLocation @(Select LetPattern ast1) @(Select LetPattern ast2) p
         in LetIn
                (stripLocation v)
                p'
                (stripExprLocation e1)
                (stripExprLocation e2)
    stripExprLocation' (Let v p e) =
        let p' = stripLocation @(Select LetPattern ast1) @(Select LetPattern ast2) p
         in Let (stripLocation v) p' (stripExprLocation e)
    stripExprLocation' (Block b) = Block (stripExprLocation <$> b)
    stripExprLocation' (Tuple t) =
        let t' = dataConAs @(Select Tuple ast1) @(NonEmpty (Expr ast1)) t
         in Tuple $ asDataCon (stripExprLocation <$> t')
    stripExprLocation' (InParens e) =
        let e' = dataConAs @(Select InParens ast1) @(Expr ast1) e
         in InParens $ asDataCon (stripExprLocation e')

instance
    forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
    ( ASTLocate' ast1 ~ Located
    , ASTLocate' ast2 ~ Unlocated
    , ( ApplyAsFunctorish
            (Select PatternType ast1)
            (Select PatternType ast2)
            (Type ast1)
            (Type ast2)
      )
    , ApplyAsFunctorish
        (Select ConsPattern ast1)
        (Select ConsPattern ast2)
        (Pattern ast1)
        (Pattern ast2)
    , ApplyAsFunctorish
        (Select ListPattern ast1)
        (Select ListPattern ast2)
        (Pattern ast1)
        (Pattern ast2)
    , ApplyAsFunctorish
        (Select TuplePattern ast1)
        (Select TuplePattern ast2)
        (Pattern ast1)
        (Pattern ast2)
    , StripLocation
        (CleanupLocated (Located (Select TupleType ast1)))
        (Select TupleType ast1)
    , ( StripLocation
            (CleanupLocated (Located (Select ASTTypeVar ast1)))
            (Select ASTTypeVar ast2)
      )
    , ( StripLocation
            (CleanupLocated (Located (Select VarPat ast1)))
            (Select VarPat ast2)
      )
    , ( StripLocation
            (CleanupLocated (Located (Select ConPat ast1)))
            (Select ConPat ast2)
      )
    , (StripLocation (CleanupLocated (Located (Select UserDefinedType ast1))) (Select UserDefinedType ast2))
    , StripLocation (Select TypeKind ast1) (Select TypeKind ast2)
    , StripLocation (Type ast1) (Type ast2)
    , DataConAs
        (Select TupleType ast1)
        (AtLeast2List (Type ast1))
    , DataConAs
        (Select TupleType ast2)
        (AtLeast2List (Type ast2))
    , StripLocation
        (Select TupleType ast1)
        (Select TupleType ast2)
    ) =>
    StripLocation (Pattern ast1) (Pattern ast2)
    where
    stripLocation = stripPatternLocation @ast1 @ast2

stripPatternLocation ::
    forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
    ( ASTLocate' ast1 ~ Located
    , ASTLocate' ast2 ~ Unlocated
    , StripLocation (Type ast1) (Type ast2)
    , _
    ) =>
    Pattern ast1 ->
    Pattern ast2
stripPatternLocation (Pattern (p :: ASTLocate ast1 (Pattern' ast1), t)) =
    let p' = fmapUnlocated @LocatedAST @ast1 stripPatternLocation' p
     in Pattern
            ( stripLocation p'
            , applyAsFunctorish @(Select PatternType ast1) @(Select PatternType ast2) @(Type ast1) @(Type ast2) stripTypeLocation t
            )
  where
    stripPatternLocation' :: Pattern' ast1 -> Pattern' ast2
    stripPatternLocation' (VarPattern v) = VarPattern (stripLocation v)
    stripPatternLocation' (ConstructorPattern c ps) = ConstructorPattern (stripLocation c) (stripPatternLocation <$> ps)
    stripPatternLocation' (ListPattern l) =
        ListPattern
            ( applyAsFunctorish @(Select ListPattern ast1)
                @(Select ListPattern ast2)
                @(Pattern ast1)
                @(Pattern ast2)
                stripPatternLocation
                l
            )
    stripPatternLocation' (ConsPattern p1) =
        ConsPattern
            ( applyAsFunctorish @(Select ConsPattern ast1)
                @(Select ConsPattern ast2)
                @(Pattern ast1)
                @(Pattern ast2)
                stripPatternLocation
                p1
            )
    stripPatternLocation' (TuplePattern ps) =
        TuplePattern
            ( applyAsFunctorish @(Select TuplePattern ast1) @(Select TuplePattern ast2) @(Pattern ast1)
                @(Pattern ast2)
                stripPatternLocation
                ps
            )
    stripPatternLocation' WildcardPattern = WildcardPattern
    stripPatternLocation' (IntegerPattern i) = IntegerPattern i
    stripPatternLocation' (FloatPattern f) = FloatPattern f
    stripPatternLocation' (StringPattern s) = StringPattern s
    stripPatternLocation' (CharPattern c) = CharPattern c
    stripPatternLocation' UnitPattern = UnitPattern

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
    , StripLocation (CleanupLocated (Located (Select ASTTypeVar ast1))) (Select ASTTypeVar ast2)
    , StripLocation (CleanupLocated (Located (Select UserDefinedType ast1))) (Select UserDefinedType ast2)
    , StripLocation (Select TypeKind ast1) (Select TypeKind ast2)
    , StripLocation (Select TupleType ast1) (Select TupleType ast2)
    , StripLocation
        (CleanupLocated (Located (Select TupleType ast1)))
        (Select TupleType ast1)
    , DataConAs
        (Select TupleType ast1)
        (AtLeast2List (Type ast1))
    , DataConAs
        (Select TupleType ast2)
        (AtLeast2List (Type ast2))
    ) =>
    StripLocation (Type ast1) (Type ast2)
    where
    stripLocation = stripTypeLocation @ast1 @ast2

stripTypeLocation ::
    forall (ast1 :: LocatedAST) (ast2 :: UnlocatedAST).
    ( (ASTLocate' ast1 ~ Located)
    , ASTLocate' ast2 ~ Unlocated
    , StripLocation (CleanupLocated (Located (Select ASTTypeVar ast1))) (Select ASTTypeVar ast2)
    , StripLocation (CleanupLocated (Located (Select UserDefinedType ast1))) (Select UserDefinedType ast2)
    , StripLocation (CleanupLocated (Located (Select TupleType ast1))) (Select TupleType ast1)
    , StripLocation (Select TypeKind ast1) (Select TypeKind ast2)
    , DataConAs (Select TupleType ast1) (AtLeast2List (Type ast1))
    , DataConAs (Select TupleType ast2) (AtLeast2List (Type ast2))
    ) =>
    Type ast1 ->
    Type ast2
stripTypeLocation (Type (t :: ASTLocate ast1 (Type' ast1), kind)) =
    let t' = fmapUnlocated @LocatedAST @ast1 stripTypeLocation' t
     in Type (stripLocation @(ASTLocate ast1 (Type' ast2)) @(Type' ast2) t', stripLocation kind)
  where
    stripTypeLocation' :: Type' ast1 -> Type' ast2
    stripTypeLocation' (TypeVar name) = TypeVar (stripLocation name)
    stripTypeLocation' (FunctionType a b) = FunctionType (stripTypeLocation a) (stripTypeLocation b)
    stripTypeLocation' UnitType = UnitType
    stripTypeLocation' (TypeConstructorApplication a b) = TypeConstructorApplication (stripTypeLocation a) (stripTypeLocation b)
    stripTypeLocation' (ListType a) = ListType (stripTypeLocation a)
    stripTypeLocation' (TupleType a) =
        let t' = dataConAs @(Select TupleType ast1) @(AtLeast2List (Type ast1)) a
         in TupleType $ asDataCon ((stripTypeLocation <$> t') :: AtLeast2List (Type ast2))
    stripTypeLocation' (UserDefinedType n) = UserDefinedType (stripLocation n)
    stripTypeLocation' (RecordType r) = RecordType (bimapF stripLocation stripTypeLocation r)
