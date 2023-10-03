{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Elara.AST.Generic.Instances.Simple where

import Data.Data
import Data.Kind qualified as Kind
import Elara.AST.Generic.Types
import Elara.AST.Name

type ForAllExpr :: (Kind.Type -> Kind.Constraint) -> ast -> Kind.Constraint
type ForAllExpr c ast =
    ( c (Expr ast)
    , c (ASTLocate ast (Select "VarRef" ast))
    , c (ASTLocate ast (Select "LambdaPattern" ast))
    , c (ASTLocate ast (Select "ConRef" ast))
    , c (ASTLocate ast (Select "LetParamName" ast))
    )

-- Eq instances

deriving instance
    ( (Eq (Select "LetPattern" ast))
    , ForAllExpr Eq ast
    , (Eq (ASTLocate ast (BinaryOperator' ast)))
    , (Eq (Select "ExprType" ast))
    , (Eq (Select "PatternType" ast))
    , (Eq (Select "BinaryOperator" ast))
    , Eq (Select "TypeApplication" ast)
    , Eq (ASTLocate ast (Expr' ast))
    , Eq (ASTLocate ast (Pattern' ast))
    , Eq (Type ast)
    ) =>
    Eq (Expr' ast)

deriving instance (Eq (ASTLocate ast (Expr' ast)), Eq (Select "ExprType" ast)) => Eq (Expr ast)

deriving instance
    ( Eq (ASTLocate ast (Select "VarPat" ast))
    , Eq (ASTLocate ast (Select "ConPat" ast))
    , (Eq (Select "PatternType" ast))
    , Eq (ASTLocate ast (Pattern' ast))
    ) =>
    Eq (Pattern' ast)

deriving instance (Eq (ASTLocate ast (Pattern' ast)), Eq (Select "PatternType" ast)) => Eq (Pattern ast)

deriving instance
    ( Eq (ASTLocate ast (Select "TypeVar" ast))
    , Eq (ASTLocate ast (Select "UserDefinedType" ast))
    , Eq (ASTLocate ast (Type' ast))
    , Eq (ASTLocate ast LowerAlphaName)
    , Eq (Type ast)
    ) =>
    Eq (Type' ast)

deriving instance (Eq (ASTLocate ast (Type' ast))) => Eq (Type ast)

deriving instance
    ( Eq (ASTLocate ast (Select "SymOp" ast))
    , Eq (Select "Infixed" ast)
    ) =>
    Eq (BinaryOperator' ast)

deriving instance (Eq (ASTLocate ast (BinaryOperator' ast))) => Eq (BinaryOperator ast)

-- Show instances

deriving instance
    ( (Show (Select "LetPattern" ast))
    , (Show (ASTLocate ast (Select "VarRef" ast)))
    , (Show (ASTLocate ast (Select "LambdaPattern" ast)))
    , (Show (ASTLocate ast (Select "ConRef" ast)))
    , (Show (ASTLocate ast (Select "LetParamName" ast)))
    , (Show (Select "TypeApplication" ast))
    , (Show (ASTLocate ast (BinaryOperator' ast)))
    , (Show (Select "ExprType" ast))
    , (Show (Select "PatternType" ast))
    , Show (Select "BinaryOperator" ast)
    , Show (ASTLocate ast (Expr' ast))
    , Show (ASTLocate ast (Pattern' ast))
    , Show (Type ast)
    ) =>
    Show (Expr' ast)

deriving instance (Show (ASTLocate ast (Expr' ast)), Show (Select "ExprType" ast)) => Show (Expr ast)

deriving instance
    ( Show (ASTLocate ast (Select "VarPat" ast))
    , Show (ASTLocate ast (Select "ConPat" ast))
    , (Show (Select "PatternType" ast))
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

deriving instance (Show (ASTLocate ast (Type' ast))) => Show (Type ast)

deriving instance
    ( Show (ASTLocate ast (Select "SymOp" ast))
    , Show (Select "Infixed" ast)
    ) =>
    Show (BinaryOperator' ast)

deriving instance (Show (ASTLocate ast (BinaryOperator' ast))) => Show (BinaryOperator ast)

deriving instance
    ( Show (DeclarationBody ast)
    , Show (ASTLocate ast (Select "DeclarationName" ast))
    , Show (ASTLocate ast ModuleName)
    ) =>
    Show (Declaration' ast)

deriving instance (Show (ASTLocate ast (Declaration' ast))) => Show (Declaration ast)

deriving instance
    ( (Show (Select "ValueTypeDef" ast))
    , (Show (Select "ValuePatterns" ast))
    , (Show (Select "ValueType" ast))
    , Show (Select "ExprType" ast)
    , Show (ASTLocate ast (Select "TypeVar" ast))
    , Show (ASTLocate ast (Expr' ast))
    , Show (ASTLocate ast (TypeDeclaration ast))
    ) =>
    Show (DeclarationBody' ast)

deriving instance (Show (ASTLocate ast (DeclarationBody' ast))) => Show (DeclarationBody ast)

deriving instance
    ( Show (ASTLocate ast (Select "ConstructorName" ast))
    , Show (Type ast)
    ) =>
    Show (TypeDeclaration ast)

-- Ord instances

deriving instance (Ord (ASTLocate ast (BinaryOperator' ast))) => Ord (BinaryOperator ast)

deriving instance
    ( Ord (ASTLocate ast (Select "SymOp" ast))
    , Ord (Select "Infixed" ast)
    ) =>
    Ord (BinaryOperator' ast)

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
    , (Data (Select "ExprType" ast))
    , Data (ASTLocate ast (Select "VarRef" ast))
    , Data (ASTLocate ast (Select "ConRef" ast))
    , Data (ASTLocate ast (Select "LetParamName" ast))
    , Data (ASTLocate ast (Select "LambdaPattern" ast))
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
    , Typeable ast
    , Typeable a
    ) =>
    Data (Type' ast)
