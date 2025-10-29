{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Select where

-- type LocatedAST :: AST
data LocatedAST = Frontend | Desugared | Renamed | Shunted | MidKinded | Kinded | Typed | Core

-- type UnlocatedAST :: AST
data UnlocatedAST = UnlocatedFrontend | UnlocatedDesugared | UnlocatedRenamed | UnlocatedShunted | UnlocatedTyped

type data ASTSelector
    = ASTVarRef
    | ConRef
    | LambdaPattern
    | TypeApplication
    | PatternType
    | ASTBinaryOperator
    | List
    | LetParamName
    | LetPattern
    | ConstructorName
    | ADTParam
    | Alias
    | Annotations ForSelector
    | KindAnnotation
    | ASTTypeVar
    | TypeKind
    | UserDefinedType
    | InfixDecl
    | ASTName ForSelector
    | Patterns ForSelector
    | ASTType ForSelector
    | AnyName
    | VarPat
    | ConPat
    | AnnotationName
    | ValueTypeDef
    | Tuple
    | InParens
    | ListPattern
    | TuplePattern
    | ConsPattern
    | SymOp
    | Infixed

type data ForSelector
    = ForType
    | ForTypeDecl
    | ForValueDecl
    | ForExpr
