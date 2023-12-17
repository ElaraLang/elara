{- | Renamed AST Type
This is very similar to 'Elara.AST.Desugared.Expr'' except everything is renamed to be unambiguous.
-}
module Elara.AST.Renamed where

import Elara.AST.Generic (ASTLocate', ASTQual, InfixDeclaration, Select)
import Elara.AST.Generic qualified as Generic
import Elara.AST.Generic.Common
import Elara.AST.Name (LowerAlphaName, Name, OpName, Qualified, TypeName, VarName, VarOrConName)
import Elara.AST.Region (Located (..))
import Elara.AST.Select (LocatedAST (Renamed))
import Elara.AST.VarRef (VarRef)
import Elara.Data.Unique (Unique)

type instance ASTLocate' 'Renamed = Located

type instance ASTQual 'Renamed = Qualified

-- Selections for 'Expr'
type instance Select "ExprType" 'Renamed = Maybe RenamedType

type instance Select "LambdaPattern" 'Renamed = Unique VarName

type instance Select "LetPattern" 'Renamed = NoFieldValue

type instance Select "VarRef" 'Renamed = VarRef VarName

type instance Select "ConRef" 'Renamed = Qualified TypeName

type instance Select "SymOp" 'Renamed = VarRef OpName

type instance Select "Infixed" 'Renamed = VarRef VarOrConName

type instance Select "LetParamName" 'Renamed = Unique VarName

type instance Select "InParens" 'Renamed = RenamedExpr

type instance Select "BinaryOperator" 'Renamed = (RenamedBinaryOperator, RenamedExpr, RenamedExpr)

type instance Select "PatternType" 'Renamed = Maybe RenamedType

type instance Select "VarPat" 'Renamed = Unique LowerAlphaName

type instance Select "ConPat" 'Renamed = Qualified TypeName

type instance Select "TypeApplication" 'Renamed = RenamedType

-- Selections for 'DeclarationBody'
type instance Select "ValuePatterns" 'Renamed = NoFieldValue

type instance Select "ValueType" 'Renamed = Maybe RenamedType

type instance Select "ValueTypeDef" 'Renamed = DataConCantHappen

type instance Select "ValueDeclAnnotations" 'Renamed = RenamedValueDeclAnnotations
type instance Select "TypeDeclAnnotations" 'Renamed = RenamedTypeDeclAnnotations

type instance Select "InfixDecl" 'Renamed = DataConCantHappen

newtype RenamedValueDeclAnnotations = RenamedValueDeclAnnotations
    { infixValueDecl :: Maybe (InfixDeclaration Renamed)
    }
    deriving (Show)

newtype RenamedTypeDeclAnnotations = RenamedTypeDeclAnnotations
    { infixTypeDecl :: Maybe (InfixDeclaration Renamed)
    }
    deriving (Show)

-- Selections for 'Declaration'
type instance Select "DeclarationName" 'Renamed = Qualified Name

-- Selections for 'Type'
type instance Select "TypeVar" 'Renamed = Unique LowerAlphaName

type instance Select "UserDefinedType" 'Renamed = Qualified TypeName

type instance Select "ConstructorName" 'Renamed = Qualified TypeName

type RenamedExpr = Generic.Expr 'Renamed

type RenamedExpr' = Generic.Expr' 'Renamed

type RenamedPattern = Generic.Pattern 'Renamed

type RenamedPattern' = Generic.Pattern' 'Renamed

type RenamedBinaryOperator = Generic.BinaryOperator 'Renamed

type RenamedBinaryOperator' = Generic.BinaryOperator' 'Renamed

type RenamedType = Generic.Type 'Renamed

type RenamedType' = Generic.Type' 'Renamed

type RenamedDeclaration = Generic.Declaration 'Renamed

type RenamedDeclaration' = Generic.Declaration' 'Renamed

type RenamedDeclarationBody = Generic.DeclarationBody 'Renamed

type RenamedDeclarationBody' = Generic.DeclarationBody' 'Renamed

type RenamedTypeDeclaration = Generic.TypeDeclaration 'Renamed
