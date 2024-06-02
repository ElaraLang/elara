{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Typed AST Type
This is very similar to 'Elara.AST.Shunted.Expr' except:

- Everything has a type!
-}
module Elara.AST.Typed where

import Data.Generics.Product
import Data.Generics.Wrapped
import Elara.AST.Generic (ASTLocate', ASTQual, Select)
import Elara.AST.Generic qualified as Generic
import Elara.AST.Generic.Common
import Elara.AST.Name (LowerAlphaName, Name (..), OpName, Qualified, TypeName (..), VarName)
import Elara.AST.Region (Located (..), SourceRegion, unlocated)
import Elara.AST.Select (LocatedAST (Typed))
import Elara.AST.VarRef (VarRef, VarRef' (..))
import Elara.Data.Kind (ElaraKind)
import Elara.Data.TopologicalGraph
import Elara.Data.Unique (Unique)
import Elara.TypeInfer.Type (Type (..))
import Optics (foldOf)

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

type instance Select "List" 'Typed = DataConCantHappen
type instance Select "Tuple" 'Typed = DataConCantHappen
type instance Select "BinaryOperator" 'Typed = DataConCantHappen

type instance Select "PatternType" 'Typed = Type SourceRegion

type instance Select "VarPat" 'Typed = Unique VarName
type instance Select "ConPat" 'Typed = Qualified TypeName
type instance Select "ListPattern" 'Typed = DataConCantHappen
type instance Select "ConsPattern" 'Typed = DataConCantHappen

type instance Select "TypeApplication" Typed = Type SourceRegion

-- Selections for 'DeclarationBody'
type instance Select "ValuePatterns" 'Typed = NoFieldValue

type instance Select "ValueType" 'Typed = NoFieldValue -- types are kept in the expression rather than declarations now

type instance Select "ValueTypeDef" 'Typed = DataConCantHappen
type instance Select "InfixDecl" 'Typed = DataConCantHappen
type instance Select "Alias" 'Typed = (Type SourceRegion, ElaraKind)
type instance Select "ADTParam" 'Typed = (Type SourceRegion, ElaraKind)

-- Selections for 'Declaration'
type instance Select "DeclarationName" 'Typed = Qualified Name

-- Selections for 'Type'
type instance Select "TypeVar" 'Typed = Unique LowerAlphaName
type instance Select "TypeKind" 'Typed = ElaraKind
type instance Select "KindAnnotation" 'Typed = ElaraKind

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

instance HasDependencies TypedDeclaration where
    type Key TypedDeclaration = Qualified Name

    keys sd =
        view (_Unwrapped % unlocated % field' @"name" % unlocated) sd :| case sd ^. _Unwrapped % unlocated % field' @"body" % _Unwrapped % unlocated of
            Generic.TypeDeclaration _ (Located _ (Generic.ADT ctors)) _ ->
                toList (NTypeName <<$>> (ctors ^.. each % _1 % unlocated))
            _ -> []
    dependencies decl = case decl ^. _Unwrapped % unlocated % field' @"body" % _Unwrapped % unlocated of
        Generic.Value e NoFieldValue NoFieldValue _ ->
            valueDependencies e
                <> patternDependencies e
        Generic.TypeDeclaration _ x _ ->
            case x of
                Located _ (Generic.ADT ctors) ->
                    concatMapOf (each % _2 % each % _1) typeDependencies ctors
                Located _ (Generic.Alias t) ->
                    concatMapOf _1 typeDependencies t

valueDependencies :: TypedExpr -> [Qualified Name]
valueDependencies x =
    concatMapOf (cosmosOn (_Unwrapped % _1 % unlocated)) names x
        <> concatMapOf (cosmosOnOf (_Unwrapped % _2) gplate) typeDependencies x
  where
    names :: TypedExpr' -> [Qualified Name]
    names (Generic.Var (Located _ (Global e))) = NVarName <<$>> [e ^. unlocated]
    names (Generic.Constructor (Located _ e)) = [NTypeName <$> e]
    names _ = []

patternDependencies :: TypedExpr -> [Qualified Name]
patternDependencies =
    foldOf (gplate % to patternDependencies')
  where
    patternDependencies' :: TypedPattern -> [Qualified Name]
    patternDependencies' = concatMapOf (cosmosOnOf (_Unwrapped % _1 % unlocated) gplate) names
    names :: TypedPattern' -> [Qualified Name]
    names (Generic.ConstructorPattern (Located _ e) _) = [NTypeName <$> e]
    names _ = []

typeDependencies :: Type SourceRegion -> [Qualified Name]
typeDependencies =
    concatMapOf cosmos names
  where
    names :: Type SourceRegion -> [Qualified Name]
    names (Custom{conName}) = [NTypeName . TypeName <$> conName]
    names _ = []
