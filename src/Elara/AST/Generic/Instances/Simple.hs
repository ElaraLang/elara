{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Elara.AST.Generic.Instances.Simple where

import Data.Data
import Data.Generics.Wrapped
import Data.Kind qualified as Kind
import Elara.AST.Generic.Types
import Elara.AST.Name
import Elara.AST.Region

type ForAllExpr :: (Kind.Type -> Kind.Constraint) -> ast -> Kind.Constraint
type ForAllExpr c ast =
    ( c (Expr ast)
    , c (ASTLocate ast (Select "VarRef" ast))
    , c (ASTLocate ast (Select "LambdaPattern" ast))
    , c (ASTLocate ast (Select "ConRef" ast))
    , c (ASTLocate ast (Select "LetParamName" ast))
    , c (Select "InParens" ast)
    )

-- Eq/Ord instances

deriving instance
    ( (Eq (Select "LetPattern" ast))
    , ForAllExpr Eq ast
    , (Eq (ASTLocate ast (BinaryOperator' ast)))
    , Eq (Select "ExprType" ast)
    , Eq (Select "PatternType" ast)
    , Eq (Select "BinaryOperator" ast)
    , Eq (Select "List" ast)
    , Eq (Select "Tuple" ast)
    , Eq (Select "TypeApplication" ast)
    , Eq (ASTLocate ast (Expr' ast))
    , Eq (ASTLocate ast (Pattern' ast))
    , Eq (Type ast)
    ) =>
    Eq (Expr' ast)

deriving instance
    ( Ord (Select "LetPattern" ast)
    , ForAllExpr Ord ast
    , Ord (ASTLocate ast (BinaryOperator' ast))
    , Ord (Select "ExprType" ast)
    , Ord (Select "PatternType" ast)
    , Ord (Select "BinaryOperator" ast)
    , Ord (Select "List" ast)
    , Ord (Select "Tuple" ast)
    , Ord (Select "TypeApplication" ast)
    , Ord (ASTLocate ast (Expr' ast))
    , Ord (ASTLocate ast (Pattern' ast))
    , Ord (Type ast)
    ) =>
    Ord (Expr' ast)

deriving instance (Eq (ASTLocate ast (Expr' ast)), Eq (Select "ExprType" ast)) => Eq (Expr ast)
deriving instance (Ord (ASTLocate ast (Expr' ast)), Ord (Select "ExprType" ast)) => Ord (Expr ast)

deriving instance
    ( Eq (ASTLocate ast (Select "VarPat" ast))
    , Eq (ASTLocate ast (Select "ConPat" ast))
    , Eq (Select "PatternType" ast)
    , Eq (Select "ConsPattern" ast)
    , Eq (Select "ListPattern" ast)
    , Eq (ASTLocate ast (Pattern' ast))
    ) =>
    Eq (Pattern' ast)

deriving instance
    ( Ord (ASTLocate ast (Select "VarPat" ast))
    , Ord (ASTLocate ast (Select "ConPat" ast))
    , Ord (Select "PatternType" ast)
    , Ord (Select "ConsPattern" ast)
    , Ord (Select "ListPattern" ast)
    , Ord (ASTLocate ast (Pattern' ast))
    ) =>
    Ord (Pattern' ast)

deriving instance (Eq (ASTLocate ast (Pattern' ast)), Eq (Select "PatternType" ast)) => Eq (Pattern ast)
deriving instance (Ord (ASTLocate ast (Pattern' ast)), Ord (Select "PatternType" ast)) => Ord (Pattern ast)

deriving instance
    ( Eq (ASTLocate ast (Select "TypeVar" ast))
    , Eq (ASTLocate ast (Select "UserDefinedType" ast))
    , Eq (ASTLocate ast (Type' ast))
    , Eq (ASTLocate ast LowerAlphaName)
    , Eq (Type ast)
    ) =>
    Eq (Type' ast)

deriving instance (Eq (ASTLocate ast (Type' ast)), Eq (Select "TypeKind" ast)) => Eq (Type ast)

deriving instance
    ( Ord (ASTLocate ast (Select "TypeVar" ast))
    , Ord (ASTLocate ast (Select "UserDefinedType" ast))
    , Ord (ASTLocate ast (Type' ast))
    , Ord (ASTLocate ast LowerAlphaName)
    , Ord (Type ast)
    ) =>
    Ord (Type' ast)

deriving instance (Ord (ASTLocate ast (Type' ast)), Ord (Select "TypeKind" ast)) => Ord (Type ast)

deriving instance
    ( Eq (ASTLocate ast (Select "SymOp" ast))
    , Eq (Select "Infixed" ast)
    ) =>
    Eq (BinaryOperator' ast)

deriving instance Eq (ASTLocate ast (BinaryOperator' ast)) => Eq (BinaryOperator ast)

deriving instance Eq (ASTLocate ast (Declaration' ast)) => Eq (Declaration ast)

deriving instance
    ( Eq (DeclarationBody ast)
    , Eq (ASTLocate ast (Select "DeclarationName" ast))
    , Eq (ASTLocate ast ModuleName)
    ) =>
    Eq (Declaration' ast)

deriving instance Eq (ASTLocate ast (DeclarationBody' ast)) => Eq (DeclarationBody ast)
deriving instance Ord (ASTLocate ast (BinaryOperator' ast)) => Ord (BinaryOperator ast)

deriving instance
    ( Ord (ASTLocate ast (Select "SymOp" ast))
    , Ord (Select "Infixed" ast)
    ) =>
    Ord (BinaryOperator' ast)

deriving instance
    ( Eq (Declaration' ast)
    , Ord (DeclarationBody ast)
    , Ord (ASTLocate ast (Select "DeclarationName" ast))
    , Ord (ASTLocate ast ModuleName)
    ) =>
    Ord (Declaration' ast)
deriving instance Ord (ASTLocate ast (Declaration' ast)) => Ord (Declaration ast)

deriving instance
    ( Eq (ASTLocate ast (DeclarationBody' ast))
    , Ord (ASTLocate ast (DeclarationBody' ast))
    ) =>
    Ord (DeclarationBody ast)

deriving instance
    ( Eq (DeclarationBody' ast)
    , Ord (Select "InfixDecl" ast)
    , Ord (Select "ValueTypeDef" ast)
    , Ord (Select "ValuePatterns" ast)
    , Ord (Select "ValueType" ast)
    , Ord (Expr ast)
    , Ord (ValueDeclAnnotations ast)
    , Ord (TypeDeclAnnotations ast)
    , Ord (ASTLocate ast (Select "TypeVar" ast))
    , Ord (ASTLocate ast (Select "TypeName" ast))
    , Ord (ASTLocate ast (Select "ValueName" ast))
    , Ord (ASTLocate ast (TypeDeclaration ast))
    ) =>
    Ord (DeclarationBody' ast)

deriving instance
    ( Eq (Select "InfixDecl" ast)
    , Eq (Select "ValueTypeDef" ast)
    , Eq (Select "ValuePatterns" ast)
    , Eq (Select "ValueType" ast)
    , Eq (Expr ast)
    , Eq (ValueDeclAnnotations ast)
    , Eq (TypeDeclAnnotations ast)
    , Eq (ASTLocate ast (Select "TypeVar" ast))
    , Eq (ASTLocate ast (Select "TypeName" ast))
    , Eq (ASTLocate ast (Select "ValueName" ast))
    , Eq (ASTLocate ast (TypeDeclaration ast))
    ) =>
    Eq (DeclarationBody' ast)

deriving instance (Eq (InfixDeclaration ast), Eq (Select "KindAnnotation" ast)) => Eq (TypeDeclAnnotations ast)
deriving instance
    ( Eq (ASTLocate ast (Select "AnyName" ast))
    , Eq (ASTLocate ast Int)
    , Eq (ASTLocate ast AssociativityType)
    ) =>
    Eq (InfixDeclaration ast)

deriving instance (Ord (InfixDeclaration ast), Ord (Select "KindAnnotation" ast)) => Ord (TypeDeclAnnotations ast)
deriving instance
    ( Ord (ASTLocate ast (Select "AnyName" ast))
    , Ord (ASTLocate ast Int)
    , Ord (ASTLocate ast AssociativityType)
    ) =>
    Ord (InfixDeclaration ast)

deriving instance Eq (InfixDeclaration ast) => Eq (ValueDeclAnnotations ast)

deriving instance Ord (InfixDeclaration ast) => Ord (ValueDeclAnnotations ast)

deriving instance
    ( Ord (Select "Alias" ast)
    , Eq (TypeDeclaration ast)
    , Ord (Select "ADTParam" ast)
    , Ord (ASTLocate ast (Select "ConstructorName" ast))
    ) =>
    Ord (TypeDeclaration ast)
deriving instance
    ( Eq (Select "Alias" ast)
    , Eq (Select "ADTParam" ast)
    , Eq (ASTLocate ast (Select "ConstructorName" ast))
    ) =>
    Eq (TypeDeclaration ast)

-- Show instances

deriving instance
    ( Show (Select "LetPattern" ast)
    , Show (Select "TypeApplication" ast)
    , Show (ASTLocate ast (BinaryOperator' ast))
    , Show (Select "ExprType" ast)
    , Show (Select "PatternType" ast)
    , Show (Select "BinaryOperator" ast)
    , Show (Select "List" ast)
    , Show (Select "Tuple" ast)
    , Show (ASTLocate ast (Expr' ast))
    , Show (ASTLocate ast (Pattern' ast))
    , Show (Type ast)
    , ForAllExpr Show ast
    ) =>
    Show (Expr' ast)

deriving instance (Show (ASTLocate ast (Expr' ast)), Show (Select "ExprType" ast)) => Show (Expr ast)

deriving instance
    ( Show (ASTLocate ast (Select "VarPat" ast))
    , Show (ASTLocate ast (Select "ConPat" ast))
    , Show (Select "PatternType" ast)
    , Show (Select "ConsPattern" ast)
    , Show (Select "ListPattern" ast)
    , Show (ASTLocate ast (Pattern' ast))
    ) =>
    Show (Pattern' ast)

deriving instance (Show (ASTLocate ast (Pattern' ast)), Show (Select "PatternType" ast)) => Show (Pattern ast)

deriving instance
    ( Show (ASTLocate ast (Select "TypeVar" ast))
    , Show (ASTLocate ast (Select "UserDefinedType" ast))
    , Show (ASTLocate ast (Type' ast))
    , Show (ASTLocate ast LowerAlphaName)
    , Show (Type ast)
    ) =>
    Show (Type' ast)

deriving instance (Show (ASTLocate ast (Type' ast)), Show (Select "TypeKind" ast)) => Show (Type ast)

deriving instance
    ( Show (ASTLocate ast (Select "SymOp" ast))
    , Show (Select "Infixed" ast)
    ) =>
    Show (BinaryOperator' ast)

deriving instance Show (ASTLocate ast (BinaryOperator' ast)) => Show (BinaryOperator ast)

deriving instance
    ( Show (DeclarationBody ast)
    , Show (ASTLocate ast (Select "DeclarationName" ast))
    , Show (ASTLocate ast ModuleName)
    ) =>
    Show (Declaration' ast)

deriving instance Show (ASTLocate ast (Declaration' ast)) => Show (Declaration ast)

deriving instance
    ( (Show (Select "ValueTypeDef" ast))
    , (Show (Select "ValuePatterns" ast))
    , (Show (Select "ValueType" ast))
    , Show (Select "InfixDecl" ast)
    , Show (Select "ExprType" ast)
    , Show (ASTLocate ast (Select "TypeVar" ast))
    , Show (ASTLocate ast (Select "TypeName" ast))
    , Show (ASTLocate ast (Select "ValueName" ast))
    , Show (ASTLocate ast (Expr' ast))
    , Show (ASTLocate ast Int)
    , Show (ASTLocate ast AssociativityType)
    , Show (ASTLocate ast (TypeDeclaration ast))
    , Show (TypeDeclAnnotations ast)
    , Show (ValueDeclAnnotations ast)
    ) =>
    Show (DeclarationBody' ast)

deriving instance Show (ASTLocate ast (DeclarationBody' ast)) => Show (DeclarationBody ast)

deriving instance (Show (ASTLocate ast (Select "AnyName" ast)), Show (ASTLocate ast Int), Show (ASTLocate ast AssociativityType)) => Show (InfixDeclaration ast)

deriving instance (Show (InfixDeclaration ast), Show (Select "KindAnnotation" ast)) => Show (TypeDeclAnnotations ast)
deriving instance Show (InfixDeclaration ast) => Show (ValueDeclAnnotations ast)

deriving instance
    ( Show (ASTLocate ast (Select "ConstructorName" ast))
    , Show (Type ast)
    , Show (Select "Alias" ast)
    , Show (Select "ADTParam" ast)
    ) =>
    Show (TypeDeclaration ast)

-- Data instances

deriving instance
    forall a (ast :: a).
    ( Data (ASTLocate ast (Pattern' ast))
    , Data (Select "PatternType" ast)
    , Typeable ast
    , Typeable a
    ) =>
    Data (Pattern ast)

deriving instance
    forall a (ast :: a).
    ( Typeable a
    , Typeable ast
    , (Data (Pattern ast))
    , (Data (ASTLocate ast (Select "VarPat" ast)))
    , (Data (ASTLocate ast (Select "ConPat" ast)))
    , (Data (Select "ConsPattern" ast))
    , (Data (Select "ListPattern" ast))
    ) =>
    Data (Pattern' ast)

deriving instance
    forall a (ast :: a).
    ( Data (ASTLocate ast (Expr' ast))
    , Data (Select "ExprType" ast)
    , Typeable ast
    , Typeable a
    ) =>
    Data (Expr ast)

deriving instance
    forall a (ast :: a).
    ( Data (ASTLocate ast (Expr' ast))
    , Data (Select "LetPattern" ast)
    , Data (Select "PatternType" ast)
    , Data (Select "BinaryOperator" ast)
    , Data (Select "List" ast)
    , Data (Select "Tuple" ast)
    , Data (Select "ExprType" ast)
    , ForAllExpr Data ast
    , Data (Select "TypeApplication" ast)
    , Data (ASTLocate ast (Pattern' ast))
    , Typeable ast
    , Typeable a
    ) =>
    Data (Expr' ast)

deriving instance
    forall a (ast :: a).
    ( Data (ASTLocate ast (Type' ast))
    , Data (Select "TypeVar" ast)
    , Data (Select "UserDefinedType" ast)
    , Data (Select "TypeKind" ast)
    , Typeable ast
    , Typeable a
    ) =>
    Data (Type ast)

deriving instance
    forall a (ast :: a).
    ( Data (ASTLocate ast (Type' ast))
    , Data (ASTLocate ast (Select "TypeVar" ast))
    , Data (Select "TypeVar" ast)
    , Data (ASTLocate ast (Select "UserDefinedType" ast))
    , Data (ASTLocate ast LowerAlphaName)
    , Data (Select "UserDefinedType" ast)
    , Data (Select "TypeKind" ast)
    , Typeable ast
    , Typeable a
    ) =>
    Data (Type' ast)

-- HasSourceRegion instances

instance ASTLocate' ast ~ Located => HasSourceRegion (Pattern ast) where
    sourceRegion = _Unwrapped % _1 % sourceRegion

instance ASTLocate' ast ~ Located => HasSourceRegion (Expr ast) where
    sourceRegion = _Unwrapped % _1 % sourceRegion

deriving instance (Eq v, Eq (Select "PatternType" ast)) => Eq (TypedLambdaParam v ast)
deriving instance (Ord v, Ord (Select "PatternType" ast)) => Ord (TypedLambdaParam v ast)
deriving instance (Show v, Show (Select "PatternType" ast)) => Show (TypedLambdaParam v ast)

