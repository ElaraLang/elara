-- Since when was there a warning for orphan type families?
{-# OPTIONS_GHC -Wno-orphans #-}

module Elara.AST.Frontend where

import Elara.AST.Generic
import Elara.AST.Generic.Common
import Elara.AST.Name (LowerAlphaName, MaybeQualified, Name, OpName, TypeName, VarName, VarOrConName)
import Elara.AST.Region (Located (..))
import Elara.AST.Select (LocatedAST (Frontend))

-- Generic location and qualification types
type instance ASTLocate' 'Frontend = Located

type instance ASTQual 'Frontend = MaybeQualified

-- Selections for 'Expr'
type instance Select "ExprType" 'Frontend = Maybe FrontendType

type instance Select "LambdaPattern" 'Frontend = [FrontendPattern]

type instance Select "LetPattern" 'Frontend = [FrontendPattern]

type instance Select "VarRef" 'Frontend = MaybeQualified VarName

type instance Select "ConRef" 'Frontend = MaybeQualified TypeName

type instance Select "LetParamName" 'Frontend = VarName

type instance Select "InParens" 'Frontend = FrontendExpr

type instance Select "List" 'Frontend = [FrontendExpr]
type instance Select "Tuple" 'Frontend = NonEmpty FrontendExpr
type instance Select "BinaryOperator" 'Frontend = (FrontendBinaryOperator, FrontendExpr, FrontendExpr)

type instance Select "TypeApplication" 'Frontend = FrontendType

-- Selections for 'BinaryOperator'
type instance Select "SymOp" 'Frontend = MaybeQualified OpName

type instance Select "Infixed" 'Frontend = Located (MaybeQualified VarOrConName)

-- Selections for 'Pattern'
type instance Select "PatternType" 'Frontend = Maybe FrontendType

type instance Select "VarPat" 'Frontend = LowerAlphaName

type instance Select "ConPat" 'Frontend = MaybeQualified TypeName
type instance Select "ListPattern" 'Frontend = [FrontendPattern]
type instance Select "ConsPattern" 'Frontend = (FrontendPattern, FrontendPattern)

-- Selections for 'DeclarationBody'
type instance Select "ValuePatterns" 'Frontend = [FrontendPattern]

type instance Select "ValueType" 'Frontend = NoFieldValue

type instance Select "ValueTypeDef" 'Frontend = FrontendType

type instance Select "Alias" 'Frontend = FrontendType
type instance Select "ADTParam" 'Frontend = FrontendType

type instance Select "ValueDeclAnnotations" 'Frontend = NoFieldValue
type instance Select "TypeDeclAnnotations" 'Frontend = NoFieldValue
type instance Select "KindAnnotation" 'Frontend = NoFieldValue

type instance Select "InfixDecl" 'Frontend = InfixDeclaration 'Frontend

-- Selections for 'Declaration'
type instance Select "DeclarationName" 'Frontend = Name
type instance Select "AnyName" 'Frontend = Name
type instance Select "TypeName" 'Frontend = TypeName
type instance Select "ValueName" 'Frontend = VarName

-- Selections for 'Type'
type instance Select "TypeVar" 'Frontend = LowerAlphaName
type instance Select "TypeKind" 'Frontend = NoFieldValue

type instance Select "UserDefinedType" 'Frontend = MaybeQualified TypeName

type instance Select "ConstructorName" 'Frontend = TypeName

type FrontendExpr = Expr 'Frontend

type FrontendExpr' = Expr' 'Frontend

type FrontendPattern = Pattern 'Frontend

type FrontendPattern' = Pattern' 'Frontend

type FrontendBinaryOperator = BinaryOperator 'Frontend

type FrontendBinaryOperator' = BinaryOperator' 'Frontend

type FrontendType = Type 'Frontend

type FrontendType' = Type' 'Frontend

type FrontendDeclaration = Declaration 'Frontend

type FrontendDeclaration' = Declaration' 'Frontend

type FrontendDeclarationBody = DeclarationBody 'Frontend

type FrontendDeclarationBody' = DeclarationBody' 'Frontend

type FrontendTypeDeclaration = TypeDeclaration 'Frontend
