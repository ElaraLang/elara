{-# LANGUAGE TypeFamilyDependencies #-}
-- Since when was there a warning for orphan type families?
{-# OPTIONS_GHC -Wno-orphans #-}

module Elara.AST.Frontend where

import Data.Kind qualified as Kind
import Elara.AST.Generic
import Elara.AST.Generic.Common
import Elara.AST.Name (LowerAlphaName, MaybeQualified, OpName, TypeName, VarName, VarOrConName)
import Elara.AST.Region (Located (..))
import Elara.AST.Select (ASTSelector (..), ForSelector (..), LocatedAST (Frontend))
import Elara.Data.AtLeast2List (AtLeast2List)

-- Generic location and qualification types
type instance ASTLocate' Frontend = Located

type instance ASTQual Frontend = MaybeQualified

type family SelectFrontend (selector :: ASTSelector) = (v :: Kind.Type) where
    SelectFrontend (ASTType ForValueDecl) = NoFieldValue
    SelectFrontend (ASTType ForExpr) = Maybe FrontendType
    SelectFrontend LambdaPattern = [FrontendPattern]
    SelectFrontend LetPattern = [FrontendPattern]
    SelectFrontend ASTVarRef = MaybeQualified VarName
    SelectFrontend ConRef = MaybeQualified TypeName
    SelectFrontend LetParamName = VarName
    SelectFrontend InParens = FrontendExpr
    SelectFrontend List = [FrontendExpr]
    SelectFrontend Tuple = AtLeast2List FrontendExpr
    SelectFrontend ASTBinaryOperator = (FrontendBinaryOperator, FrontendExpr, FrontendExpr)
    SelectFrontend TypeApplication = FrontendType
    -- Selections for 'BinaryOperator'
    SelectFrontend SymOp = MaybeQualified OpName
    SelectFrontend Infixed = Located (MaybeQualified VarOrConName)
    -- Selections for 'Pattern'
    SelectFrontend PatternType = Maybe FrontendType
    SelectFrontend VarPat = LowerAlphaName
    SelectFrontend ConPat = MaybeQualified TypeName
    SelectFrontend ListPattern = [FrontendPattern]
    SelectFrontend TuplePattern = NonEmpty FrontendPattern
    SelectFrontend ConsPattern = (FrontendPattern, FrontendPattern)
    -- Selections for 'DeclarationBody'
    SelectFrontend (Patterns ForValueDecl) = [FrontendPattern]
    -- Selections for 'Declaration'
    SelectFrontend (ASTName ForType) = TypeName
    SelectFrontend (ASTName ForValueDecl) = VarName
    -- Selections for 'Type'
    SelectFrontend ASTTypeVar = LowerAlphaName
    SelectFrontend TupleType = AtLeast2List FrontendType
    SelectFrontend TypeKind = NoFieldValue
    SelectFrontend UserDefinedType = MaybeQualified TypeName
    SelectFrontend ConstructorName = TypeName
    SelectFrontend Alias = FrontendType
    SelectFrontend ADTParam = FrontendType
    SelectFrontend ValueTypeDef = FrontendType
    -- SelectFrontend (Annotations ForExpr) = NoFieldValue
    SelectFrontend AnnotationName = MaybeQualified TypeName
    SelectFrontend (Annotations _) = [Annotation Frontend]
    SelectFrontend KindAnnotation = NoFieldValue

-- Selections for 'Expr'

type instance Select selector Frontend = SelectFrontend selector

-- type instance Select LambdaPattern Frontend = [FrontendPattern]

-- type instance Select LetPattern Frontend = [FrontendPattern]

-- type instance Select ASTVarRef Frontend = MaybeQualified VarName

-- type instance Select ConRef Frontend = MaybeQualified TypeName

-- type instance Select LetParamName Frontend = VarName

-- type instance Select InParens Frontend = FrontendExpr

-- type instance Select List Frontend = [FrontendExpr]

-- type instance Select Tuple Frontend = NonEmpty FrontendExpr

-- type instance Select ASTBinaryOperator Frontend = (FrontendBinaryOperator, FrontendExpr, FrontendExpr)

-- type instance Select TypeApplication Frontend = FrontendType

-- -- Selections for 'BinaryOperator'
-- type instance Select SymOp Frontend = MaybeQualified OpName

-- type instance Select Infixed Frontend = Located (MaybeQualified VarOrConName)

-- -- Selections for 'Pattern'
-- type instance Select PatternType Frontend = Maybe FrontendType
-- type instance Select VarPat Frontend = LowerAlphaName
-- type instance Select ConPat Frontend = MaybeQualified TypeName
-- type instance Select ListPattern Frontend = [FrontendPattern]
-- type instance Select TuplePattern Frontend = NonEmpty FrontendPattern
-- type instance Select ConsPattern Frontend = (FrontendPattern, FrontendPattern)

-- -- Selections for 'DeclarationBody'
-- type instance Select (Patterns ForValueDecl) Frontend = [FrontendPattern]

-- type instance Select (ASTType ForExpr) Frontend = NoFieldValue

-- type instance Select ValueTypeDef Frontend = FrontendType

-- type instance Select Alias Frontend = FrontendType

-- type instance Select ADTParam Frontend = FrontendType

-- type instance Select (Annotations ForExpr) Frontend = NoFieldValue

-- type instance Select (Annotations ForType) Frontend = NoFieldValue

-- type instance Select KindAnnotation Frontend = NoFieldValue

-- type instance Select InfixDecl Frontend = InfixDeclaration Frontend

-- -- Selections for 'Declaration'
-- -- type instance Select DeclarationName Frontend = Name

-- type instance Select AnyName Frontend = Name

-- type instance Select (ASTName ForType) Frontend = TypeName

-- type instance Select (ASTName ForValueDecl) Frontend = VarName

-- -- Selections for 'Type'
-- type instance Select TypeVar Frontend = LowerAlphaName

-- type instance Select TypeKind Frontend = NoFieldValue

-- type instance Select UserDefinedType Frontend = MaybeQualified TypeName

-- type instance Select ConstructorName Frontend = TypeName

type FrontendExpr = Expr Frontend

type FrontendExpr' = Expr' Frontend

type FrontendPattern = Pattern Frontend

type FrontendPattern' = Pattern' Frontend

type FrontendBinaryOperator = BinaryOperator Frontend

type FrontendBinaryOperator' = BinaryOperator' Frontend

type FrontendType = Type Frontend

type FrontendType' = Type' Frontend

type FrontendDeclaration = Declaration Frontend

type FrontendDeclaration' = Declaration' Frontend

type FrontendDeclarationBody = DeclarationBody Frontend

type FrontendDeclarationBody' = DeclarationBody' Frontend

type FrontendTypeDeclaration = TypeDeclaration Frontend
