module Elara.AST.Frontend where

import Elara.AST.Generic (ASTLocate', ASTQual, NoFieldValue, Select)
import Elara.AST.Generic qualified as Generic
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

type instance Select "BinaryOperator" 'Frontend = (FrontendBinaryOperator, FrontendExpr, FrontendExpr)

-- Selections for 'BinaryOperator'
type instance Select "SymOp" 'Frontend = MaybeQualified OpName

type instance Select "Infixed" 'Frontend = Located (MaybeQualified VarOrConName)

-- Selections for 'Pattern'
type instance Select "PatternType" 'Frontend = Maybe FrontendType

type instance Select "VarPat" 'Frontend = LowerAlphaName

type instance Select "ConPat" 'Frontend = MaybeQualified TypeName

-- Selections for 'DeclarationBody'
type instance Select "ValuePatterns" 'Frontend = [FrontendPattern]

type instance Select "ValueType" 'Frontend = NoFieldValue

type instance Select "ValueTypeDef" 'Frontend = FrontendType

-- Selections for 'Declaration'
type instance Select "DeclarationName" 'Frontend = Name

-- Selections for 'Type'
type instance Select "TypeVar" 'Frontend = LowerAlphaName

type instance Select "UserDefinedType" 'Frontend = MaybeQualified TypeName

type instance Select "ConstructorName" 'Frontend = TypeName

type FrontendExpr = Generic.Expr 'Frontend

type FrontendExpr' = Generic.Expr' 'Frontend

type FrontendPattern = Generic.Pattern 'Frontend

type FrontendPattern' = Generic.Pattern' 'Frontend

type FrontendBinaryOperator = Generic.BinaryOperator 'Frontend

type FrontendBinaryOperator' = Generic.BinaryOperator' 'Frontend

type FrontendType = Generic.Type 'Frontend

type FrontendType' = Generic.Type' 'Frontend

type FrontendDeclaration = Generic.Declaration 'Frontend

type FrontendDeclaration' = Generic.Declaration' 'Frontend

type FrontendDeclarationBody = Generic.DeclarationBody 'Frontend

type FrontendDeclarationBody' = Generic.DeclarationBody' 'Frontend

type FrontendTypeDeclaration = Generic.TypeDeclaration 'Frontend
