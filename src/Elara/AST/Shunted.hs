{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Shunted AST Type
This is very similar to 'Elara.AST.Renamed.Expr' except:

- Operators are re-shunted to match their defined precedence and associativity
- This means there's no need for an 'InParens' token anymore so that's also gone :D
- The confusing 'VarName'/'Elara.AST.Name.OpName' mess is also gone. Binary operator invocations are replaced with prefix function calls. This always uses 'VarName'

Values of these types are produced by "Elara.Shunt".
-}
module Elara.AST.Shunted where

import Elara.AST.Generic
import Elara.AST.Generic.Common
import Elara.AST.Name (LowerAlphaName, OpName, Qualified (..), TypeName, VarName)
import Elara.AST.Region (Located (..))
import Elara.AST.Select (ASTSelector (..), ForSelector (..), LocatedAST (Shunted))
import Elara.AST.VarRef (VarRef)
import Elara.Data.Unique (Unique)

type instance ASTLocate' Shunted = Located

type instance ASTQual Shunted = Qualified

-- Selections for 'Expr'
type instance Select (ASTType ForExpr) Shunted = Maybe ShuntedType

type instance Select LambdaPattern Shunted = TypedLambdaParam (Unique VarName) Shunted

type instance Select LetPattern Shunted = NoFieldValue

type instance Select ASTVarRef Shunted = VarRef VarName

type instance Select ConRef Shunted = Qualified TypeName

type instance Select SymOp Shunted = VarRef OpName

type instance Select Infixed Shunted = VarRef VarName

type instance Select LetParamName Shunted = Unique VarName

type instance Select InParens Shunted = DataConCantHappen

type instance Select List Shunted = DataConCantHappen

type instance Select Tuple Shunted = DataConCantHappen

type instance Select ASTBinaryOperator Shunted = DataConCantHappen

type instance Select PatternType Shunted = Maybe ShuntedType

type instance Select VarPat Shunted = Unique LowerAlphaName

type instance Select ConPat Shunted = Qualified TypeName

type instance Select ConsPattern Shunted = DataConCantHappen

type instance Select ListPattern Shunted = DataConCantHappen

type instance Select TuplePattern Shunted = DataConCantHappen

type instance Select TypeApplication Shunted = ShuntedType

-- Selections for 'DeclarationBody'
type instance Select (Patterns ForValueDecl) Shunted = NoFieldValue

type instance Select (ASTType ForValueDecl) Shunted = Maybe ShuntedType

type instance Select ValueTypeDef Shunted = DataConCantHappen

type instance Select KindAnnotation Shunted = NoFieldValue

type instance Select Alias Shunted = ShuntedType

type instance Select ADTParam Shunted = ShuntedType

-- Selections for 'Declaration'

type instance Select (ASTName ForType) Shunted = Qualified TypeName

type instance Select (ASTName ForExpr) Shunted = Qualified VarName
type instance Select (ASTName ForValueDecl) Shunted = Qualified VarName

-- Selections for 'Type'
type instance Select TypeKind Shunted = NoFieldValue

type instance Select ASTTypeVar Shunted = Unique LowerAlphaName
type instance Select TupleType Shunted = DataConCantHappen

type instance Select UserDefinedType Shunted = Qualified TypeName

type instance Select ConstructorName Shunted = Qualified TypeName

type instance Select AnnotationName Shunted = Qualified TypeName
type instance Select (Annotations a) Shunted = [Annotation Shunted]

type ShuntedExpr = Expr Shunted

type ShuntedExpr' = Expr' Shunted

type ShuntedPattern = Pattern Shunted

type ShuntedPattern' = Pattern' Shunted

type ShuntedBinaryOperator = BinaryOperator Shunted

type ShuntedBinaryOperator' = BinaryOperator' Shunted

type ShuntedType = Type Shunted

type ShuntedType' = Type' Shunted

type ShuntedDeclaration = Declaration Shunted

type ShuntedDeclaration' = Declaration' Shunted

type ShuntedDeclarationBody = DeclarationBody Shunted

type ShuntedDeclarationBody' = DeclarationBody' Shunted

type ShuntedTypeDeclaration = TypeDeclaration Shunted
