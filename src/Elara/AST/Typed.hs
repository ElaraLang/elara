{-# LANGUAGE UndecidableInstances #-}

{- | Typed AST Type
This is very similar to 'Elara.AST.Shunted.Expr' except:

- Everything has a type!
-}
module Elara.AST.Typed where

import Elara.AST.Generic (ASTLocate', ASTQual, Select)
import Elara.AST.Generic qualified as Generic
import Elara.AST.Generic.Common
import Elara.AST.Name (LowerAlphaName, Name, OpName, Qualified, TypeName, VarName)
import Elara.AST.Region (Located (..), SourceRegion)
import Elara.AST.Select (LocatedAST (Typed))
import Elara.AST.VarRef (VarRef)
import Elara.Data.Unique (Unique)
import Elara.TypeInfer.Type (Type)

type instance ASTLocate' 'Typed = Located

type instance ASTQual 'Typed = Qualified

-- Selections for 'Expr'
type instance Select "ExprType" 'Typed = Type SourceRegion

type instance Select "LambdaPattern" 'Typed = Unique VarName

type instance Select "LetPattern" 'Typed = NoFieldValue

type instance Select "VarRef" 'Typed = VarRef VarName

type instance Select "ConRef" 'Typed = Qualified TypeName

type instance Select "SymOp" 'Typed = VarRef OpName

type instance Select "Infixed" 'Typed = VarRef VarName

type instance Select "LetParamName" 'Typed = Unique VarName

type instance Select "InParens" 'Typed = DataConCantHappen

type instance Select "BinaryOperator" 'Typed = DataConCantHappen

type instance Select "PatternType" 'Typed = Type SourceRegion

type instance Select "VarPat" 'Typed = Unique VarName

type instance Select "ConPat" 'Typed = Qualified TypeName

type instance Select "TypeApplication" Typed = Type SourceRegion

-- Selections for 'DeclarationBody'
type instance Select "ValuePatterns" 'Typed = NoFieldValue

type instance Select "ValueType" 'Typed = NoFieldValue -- types are kept in the expression rather than declarations now

type instance Select "ValueTypeDef" 'Typed = DataConCantHappen
type instance Select "InfixDecl" 'Typed = DataConCantHappen

-- Selections for 'Declaration'
type instance Select "DeclarationName" 'Typed = Qualified Name

-- Selections for 'Type'
type instance Select "TypeVar" 'Typed = Unique LowerAlphaName

type instance Select "UserDefinedType" 'Typed = Qualified TypeName

type instance Select "ConstructorName" 'Typed = Qualified TypeName

type TypedExpr = Generic.Expr 'Typed

type TypedExpr' = Generic.Expr' 'Typed

type TypedPattern = Generic.Pattern 'Typed

type TypedPattern' = Generic.Pattern' 'Typed

type TypedBinaryOperator = Generic.BinaryOperator 'Typed

type TypedBinaryOperator' = Generic.BinaryOperator' 'Typed

type TypedType = Generic.Type 'Typed

type TypedType' = Generic.Type' 'Typed

type TypedDeclaration = Generic.Declaration 'Typed

type TypedDeclaration' = Generic.Declaration' 'Typed

type TypedDeclarationBody = Generic.DeclarationBody 'Typed

type TypedDeclarationBody' = Generic.DeclarationBody' 'Typed

type TypedTypeDeclaration = Generic.TypeDeclaration 'Typed
