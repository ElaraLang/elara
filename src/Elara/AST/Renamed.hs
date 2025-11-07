{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Renamed AST Type
This is very similar to 'Elara.AST.Desugared.Expr'' except everything is renamed to be unambiguous.
-}
module Elara.AST.Renamed where

import Elara.AST.Generic
import Elara.AST.Generic.Common
import Elara.AST.Name (LowerAlphaName, Name, OpName, Qualified, TypeName, VarName, VarOrConName)
import Elara.AST.Region (Located (..))
import Elara.AST.Select (ASTSelector (..), ForSelector (..), LocatedAST (Renamed))
import Elara.AST.VarRef (VarRef)
import Elara.Data.Unique (Unique)

type instance ASTLocate' Renamed = Located

type instance ASTQual Renamed = Qualified

-- Selections for 'Expr'
type instance Select (ASTType ForExpr) Renamed = Maybe RenamedType

type instance Select LambdaPattern Renamed = TypedLambdaParam (Unique VarName) Renamed

type instance Select LetPattern Renamed = NoFieldValue

type instance Select ASTVarRef Renamed = VarRef VarName

type instance Select ConRef Renamed = Qualified TypeName

type instance Select SymOp Renamed = VarRef OpName

type instance Select Infixed Renamed = VarRef VarOrConName

type instance Select LetParamName Renamed = Unique VarName

type instance Select InParens Renamed = RenamedExpr

type instance Select Tuple Renamed = DataConCantHappen

type instance Select List Renamed = DataConCantHappen

type instance Select ASTBinaryOperator Renamed = (RenamedBinaryOperator, RenamedExpr, RenamedExpr)

type instance Select PatternType Renamed = Maybe RenamedType

type instance Select VarPat Renamed = Unique LowerAlphaName

type instance Select ConPat Renamed = Qualified TypeName

type instance Select ConsPattern Renamed = DataConCantHappen

type instance Select ListPattern Renamed = DataConCantHappen
type instance Select TuplePattern Renamed = DataConCantHappen

type instance Select TypeApplication Renamed = RenamedType

-- Selections for 'DeclarationBody'
type instance Select (Patterns ForValueDecl) Renamed = NoFieldValue

type instance Select TypeKind Renamed = NoFieldValue

type instance Select (ASTType ForValueDecl) Renamed = Maybe RenamedType

type instance Select ValueTypeDef Renamed = DataConCantHappen

type instance Select Alias Renamed = RenamedType

type instance Select ADTParam Renamed = RenamedType

type instance Select AnnotationName Renamed = Qualified TypeName
type instance Select (Annotations x) Renamed = [Annotation Renamed]

type instance Select KindAnnotation Renamed = NoFieldValue

-- Selections for 'Declaration'

type instance Select (ASTName ForTypeDecl) Renamed = Qualified TypeName
type instance Select (ASTName ForType) Renamed = Qualified TypeName

type instance Select (ASTName ForValueDecl) Renamed = Qualified VarName
type instance Select (ASTName ForExpr) Renamed = Qualified VarName

-- Selections for 'Type'
type instance Select ASTTypeVar Renamed = Unique LowerAlphaName

type instance Select UserDefinedType Renamed = Qualified TypeName

type instance Select ConstructorName Renamed = Qualified TypeName

type RenamedExpr = Expr Renamed

type RenamedExpr' = Expr' Renamed

type RenamedPattern = Pattern Renamed

type RenamedPattern' = Pattern' Renamed

type RenamedBinaryOperator = BinaryOperator Renamed

type RenamedBinaryOperator' = BinaryOperator' Renamed

type RenamedType = Type Renamed

type RenamedType' = Type' Renamed

type RenamedDeclaration = Declaration Renamed

type RenamedDeclaration' = Declaration' Renamed

type RenamedDeclarationBody = DeclarationBody Renamed

type RenamedDeclarationBody' = DeclarationBody' Renamed

type RenamedTypeDeclaration = TypeDeclaration Renamed
