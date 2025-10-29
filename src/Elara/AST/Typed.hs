{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Typed AST Type
This is very similar to 'Elara.AST.Shunted.Expr' except:

- Everything has a type!
-}
module Elara.AST.Typed where

import Data.Generics.Product
import Data.Generics.Wrapped
import Elara.AST.Generic (ASTLocate', ASTQual, Select, TypedLambdaParam)
import Elara.AST.Generic qualified as Generic
import Elara.AST.Generic.Common
import Elara.AST.Name (Name (..), OpName, Qualified (..), TypeName (..), VarName)
import Elara.AST.Region (Located (..), SourceRegion, unlocated)
import Elara.AST.Select (ASTSelector (..), ForSelector (..), LocatedAST (Typed))
import Elara.AST.VarRef (VarRef, VarRef' (Global))
import Elara.Data.Kind (ElaraKind)
import Elara.Data.TopologicalGraph
import Elara.Data.Unique (Unique)
import Elara.TypeInfer.Type (Monotype, Type (..))
import Elara.TypeInfer.Unique
import Optics (foldOf)

type instance ASTLocate' 'Typed = Located

type instance ASTQual 'Typed = Qualified

-- Selections for 'Expr'
type instance Select (ASTType ForExpr) 'Typed = Monotype SourceRegion

type instance Select LambdaPattern 'Typed = TypedLambdaParam (Unique VarName) 'Typed

type instance Select LetPattern 'Typed = NoFieldValue

-- VarRefs may have polytypes
type instance Select ASTVarRef 'Typed = (VarRef VarName, Type SourceRegion)

type instance Select ConRef 'Typed = Qualified TypeName

type instance Select SymOp 'Typed = VarRef OpName

type instance Select Infixed 'Typed = VarRef VarName

type instance Select LetParamName 'Typed = Unique VarName

type instance Select InParens 'Typed = DataConCantHappen

type instance Select List 'Typed = DataConCantHappen

type instance Select Tuple 'Typed = DataConCantHappen

type instance Select ASTBinaryOperator 'Typed = DataConCantHappen

type instance Select PatternType 'Typed = Monotype SourceRegion

type instance Select VarPat 'Typed = Unique VarName

type instance Select ConPat 'Typed = Qualified TypeName

type instance Select ListPattern 'Typed = DataConCantHappen

type instance Select ConsPattern 'Typed = DataConCantHappen
type instance Select TuplePattern 'Typed = DataConCantHappen

type instance Select TypeApplication 'Typed = Monotype SourceRegion

-- Selections for 'DeclarationBody'
type instance Select (Patterns ForValueDecl) 'Typed = NoFieldValue

type instance Select (ASTType ForValueDecl) 'Typed = Type SourceRegion

type instance Select ValueTypeDef 'Typed = DataConCantHappen

type instance Select Alias 'Typed = (Type SourceRegion, ElaraKind)

type instance Select ADTParam 'Typed = (Monotype SourceRegion, ElaraKind)

-- Selections for 'Declaration'

type instance Select (ASTName ForType) Typed = Qualified TypeName

type instance Select (ASTName ForValueDecl) Typed = Qualified VarName

-- Selections for 'Type'
type instance Select ASTTypeVar 'Typed = UniqueTyVar

type instance Select TypeKind 'Typed = ElaraKind

type instance Select KindAnnotation 'Typed = ElaraKind

type instance Select UserDefinedType 'Typed = Qualified TypeName

type instance Select ConstructorName 'Typed = Qualified TypeName

type instance Select (Annotations a) 'Typed = NoFieldValue

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
        let theDeclName = Qualified (sd ^. Generic.declarationName % unlocated) (sd ^. _Unwrapped % unlocated % field' @"moduleName" % unlocated)
         in theDeclName :| case sd ^. _Unwrapped % unlocated % field' @"body" % _Unwrapped % unlocated of
                Generic.TypeDeclaration _ _ (Located _ (Generic.ADT ctors)) _ ->
                    toList (NTypeName <<$>> (ctors ^.. each % _1 % unlocated))
                _ -> []
    dependencies decl = case decl ^. _Unwrapped % unlocated % field' @"body" % _Unwrapped % unlocated of
        Generic.Value _ e NoFieldValue type' _ ->
            valueDependencies e
                <> patternDependencies e
                <> typeDependencies type'
        Generic.TypeDeclaration _ _ x _ ->
            case x of
                Located _ (Generic.ADT ctors) ->
                    ctors ^.. each % _2 % each % _1 % gplate @(Qualified TypeName) % to (NTypeName <$>)
                Located _ (Generic.Alias t) ->
                    t ^.. gplate @(Qualified TypeName) % to (NTypeName <$>)

valueDependencies :: TypedExpr -> [Qualified Name]
valueDependencies x =
    concatMapOf (cosmosOn (_Unwrapped % _1 % unlocated)) names x
        <> concatMapOf (cosmosOnOf (_Unwrapped % _2) gplate) monotypeDependencies x
  where
    names :: TypedExpr' -> [Qualified Name]
    names (Generic.Var (Located _ (Global e, t))) = (NVarName <<$>> [e ^. unlocated]) <> typeDependencies t
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

monotypeDependencies :: Monotype SourceRegion -> [Qualified Name]
monotypeDependencies t = t ^.. gplate @(Qualified TypeName) % to (NTypeName <$>)

typeDependencies :: Type SourceRegion -> [Qualified Name]
typeDependencies t = t ^.. gplate @(Qualified TypeName) % to (NTypeName <$>)
