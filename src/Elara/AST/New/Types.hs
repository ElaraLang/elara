{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.New.Types where

import Elara.AST.Name (LowerAlphaName, ModuleName)
import Elara.AST.New.Phase

-- | Expression node with location and metadata
data Expr loc p = Expr !loc !(ExpressionMeta p loc) (Expr' loc p)
    deriving (Generic)

-- | Expression syntax
data Expr' loc p
    = EInt Integer
    | EFloat Double
    | EString Text
    | EChar Char
    | EUnit
    | EVar (VariableExtension p) (ValueOccurrence p loc)
    | ECon (ConstructorNodeExtension p) (ConstructorOccurrence p loc)
    | ELam (LambdaExtension p) (LambdaBinder p loc) (Expr loc p)
    | EApp (ApplicationExtension p) (Expr loc p) (Expr loc p)
    | ETyApp (Expr loc p) (Type loc p)
    | EIf (Expr loc p) (Expr loc p) (Expr loc p)
    | EMatch (Expr loc p) [(Pattern loc p, Expr loc p)]
    | ELetIn (LetExtension p) (ValueBinder p loc) (Expr loc p) (Expr loc p)
    | ELet (LetExtension p) (ValueBinder p loc) (Expr loc p)
    | EBlock (NonEmpty (Expr loc p))
    | EAnn (Expr loc p) (Type loc p)
    | EExtension (ExpressionExtension p loc)
    deriving (Generic)

-- | Pattern node with location and metadata
data Pattern loc p = Pattern !loc !(PatternMeta p loc) (Pattern' loc p)
    deriving (Generic)

-- | Pattern syntax
data Pattern' loc p
    = PVar (ValueBinder p loc)
    | PCon (ConstructorOccurrence p loc) [Pattern loc p]
    | PWildcard
    | PInt Integer
    | PFloat Double
    | PString Text
    | PChar Char
    | PUnit
    | PExtension (PatternExtension p loc)
    deriving (Generic)

-- | Type node with location and metadata
data Type loc p = Type !loc !(TypeMeta p loc) (Type' loc p)
    deriving (Generic)

-- | Type syntax
data Type' loc p
    = TVar (TypeVariable p loc)
    | TFun (Type loc p) (Type loc p)
    | TUnit
    | TApp (Type loc p) (Type loc p)
    | TUserDefined (TypeOccurrence p loc)
    | TRecord (NonEmpty (Locate loc LowerAlphaName, Type loc p))
    | TList (Type loc p)
    | TExtension (TypeSyntaxExtension p loc)
    deriving (Generic)

-- | Binary operator (only exists in extension types for early phases)
data BinaryOperator loc p
    = SymOp !loc (OperatorOccurrence p loc)
    | InfixedOp !loc (InfixedOccurrence p loc)
    deriving (Generic)

-- | Type declarations (ADT or alias)
data TypeDeclaration loc p
    = ADT (NonEmpty (ConstructorBinder p loc, [Type loc p]))
    | Alias (Type loc p)
    deriving (Generic)

-- | Annotation on a declaration
data Annotation loc p = Annotation
    { annotationName :: TypeOccurrence p loc
    , annotationArgs :: [AnnotationArg loc p]
    }
    deriving (Generic)

-- | Annotation argument (must be a constant expression)
newtype AnnotationArg loc p = AnnotationArg (Expr loc p)
    deriving (Generic)

-- | Top-level declaration with location
data Declaration loc p = Declaration !loc (Declaration' loc p)
    deriving (Generic)

-- | Declaration content
data Declaration' loc p = Declaration'
    { declModuleName :: Locate loc ModuleName
    , declBody :: DeclarationBody loc p
    }
    deriving (Generic)

-- | Declaration body with location
data DeclarationBody loc p = DeclarationBody !loc (DeclarationBody' loc p)
    deriving (Generic)

-- | Lambda binder with optional type annotation, used from Renamed onward
data TypedLambdaParam v loc p = TypedLambdaParam v (PatternMeta p loc)
    deriving (Generic)

-- | Declaration body syntax
data DeclarationBody' loc p
    = ValueDeclaration
        (TopValueBinder p loc)
        (Expr loc p)
        (ValueDeclPatterns p loc)
        (ValueDeclTypeAnnotation p loc)
        (ValueDeclMetadata p loc)
        [Annotation loc p]
    | TypeDeclarationBody
        (TopTypeBinder p loc)
        [TypeVariable p loc]
        (TypeDeclaration loc p)
        (Maybe (Type loc p))
        (TypeDeclMetadata p loc)
        [Annotation loc p]
    | DeclBodyExtension (DeclBodyExtension p loc)
    deriving (Generic)
