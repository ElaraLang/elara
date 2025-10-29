-- Since when was there a warning for orphan type families?
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
This is the second main AST stage, which is very similar to the `Elara.AST.Desugared.Expr` AST, with a few key differences:

  * Lambdas only have 1 argument (ones with multiple arguments are desugared into nested lambdas)
  * Let bindings have no patterns, they are desugared into lambdas
  * Def and Let declarations are merged into a single entity
-}
module Elara.AST.Desugared where

import Elara.AST.Generic
import Elara.AST.Generic.Common
import Elara.AST.Name (LowerAlphaName, MaybeQualified, Name, OpName, TypeName, VarName, VarOrConName)
import Elara.AST.Region (Located)
import Elara.AST.Select (ASTSelector (..), ForSelector (..), LocatedAST (Desugared))

-- Generic location and qualification types
type instance ASTLocate' 'Desugared = Located

type instance ASTQual 'Desugared = MaybeQualified

-- Selections for 'Expr'
type instance Select (ASTType ForExpr) 'Desugared = Maybe DesugaredType

type instance Select LambdaPattern 'Desugared = DesugaredPattern

type instance Select LetPattern 'Desugared = NoFieldValue

type instance Select ASTVarRef 'Desugared = MaybeQualified VarName

type instance Select ConRef 'Desugared = MaybeQualified TypeName

type instance Select LetParamName 'Desugared = VarName

type instance Select InParens 'Desugared = DesugaredExpr

type instance Select List 'Desugared = [DesugaredExpr]

type instance Select Tuple 'Desugared = NonEmpty DesugaredExpr

type instance Select ASTBinaryOperator 'Desugared = (DesugaredBinaryOperator, DesugaredExpr, DesugaredExpr)

type instance Select (ASTType ForValueDecl) 'Desugared = Maybe DesugaredType

-- Selections for 'BinaryOperator'
type instance Select SymOp 'Desugared = MaybeQualified OpName

type instance Select Infixed 'Desugared = Located (MaybeQualified VarOrConName)

-- Selections for 'Pattern'
type instance Select PatternType 'Desugared = Maybe DesugaredType

type instance Select VarPat 'Desugared = LowerAlphaName

type instance Select ConPat 'Desugared = MaybeQualified TypeName

type instance Select ConsPattern 'Desugared = (DesugaredPattern, DesugaredPattern)

type instance Select ListPattern 'Desugared = [DesugaredPattern]

type instance Select TuplePattern 'Desugared = NonEmpty DesugaredPattern

type instance Select TypeApplication 'Desugared = DesugaredType

-- Selections for 'Declaration'
-- type instance Select DeclarationName 'Desugared = Name

type instance Select AnyName 'Desugared = Name

type instance Select (ASTName ForType) 'Desugared = TypeName

type instance Select (ASTName ForExpr) 'Desugared = VarName
type instance Select (ASTName ForValueDecl) 'Desugared = VarName

-- Selections for 'DeclarationBody'
type instance Select (Patterns ForValueDecl) 'Desugared = NoFieldValue

type instance Select ValueTypeDef 'Desugared = DataConCantHappen

type instance Select KindAnnotation 'Desugared = NoFieldValue

type instance Select Alias 'Desugared = DesugaredType

type instance Select ADTParam 'Desugared = DesugaredType

type instance Select (Annotations any) 'Desugared = NoFieldValue

-- Selections for 'Type'
type instance Select ASTTypeVar 'Desugared = LowerAlphaName

type instance Select TypeKind 'Desugared = NoFieldValue

type instance Select UserDefinedType 'Desugared = MaybeQualified TypeName

type instance Select ConstructorName 'Desugared = TypeName

type DesugaredExpr = Expr 'Desugared

type DesugaredExpr' = Expr' 'Desugared

type DesugaredPattern = Pattern 'Desugared

type DesugaredPattern' = Pattern' 'Desugared

type DesugaredBinaryOperator = BinaryOperator 'Desugared

type DesugaredBinaryOperator' = BinaryOperator' 'Desugared

type DesugaredType = Type 'Desugared

type DesugaredType' = Type' 'Desugared

type DesugaredDeclaration = Declaration 'Desugared

type DesugaredDeclaration' = Declaration' 'Desugared

type DesugaredDeclarationBody = DeclarationBody 'Desugared

type DesugaredDeclarationBody' = DeclarationBody' 'Desugared

type DesugaredTypeDeclaration = TypeDeclaration 'Desugared
