{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Shunted AST Type
This is very similar to 'Elara.AST.Renamed.Expr' except:

- Operators are re-shunted to match their defined precedence and associativity
- This means there's no need for an 'InParens' token anymore so that's also gone :D
- The confusing 'VarName'/'Elara.AST.Name.OpName' bs is also gone. Binary operator invocations are replaced with prefix function calls. This always uses VarName
-}
module Elara.AST.Shunted where

import Data.Generics.Product
import Data.Generics.Wrapped
import Elara.AST.Generic (ASTLocate', ASTQual, Expr' (..), Pattern' (..), Select, Type' (..))
import Elara.AST.Generic qualified as Generic
import Elara.AST.Generic.Common
import Elara.AST.Name (LowerAlphaName, Name (..), OpName, Qualified, TypeName, VarName)
import Elara.AST.Region (Located (..), unlocated)
import Elara.AST.Select (LocatedAST (Shunted))
import Elara.AST.VarRef (VarRef, VarRef' (..))
import Elara.Data.TopologicalGraph (HasDependencies (..))
import Elara.Data.Unique (Unique)
import Optics (foldOf)

type instance ASTLocate' 'Shunted = Located

type instance ASTQual 'Shunted = Qualified

-- Selections for 'Expr'
type instance Select "ExprType" 'Shunted = Maybe ShuntedType

type instance Select "LambdaPattern" 'Shunted = Unique VarName

type instance Select "LetPattern" 'Shunted = NoFieldValue

type instance Select "VarRef" 'Shunted = VarRef VarName

type instance Select "ConRef" 'Shunted = Qualified TypeName

type instance Select "SymOp" 'Shunted = VarRef OpName

type instance Select "Infixed" 'Shunted = VarRef VarName

type instance Select "LetParamName" 'Shunted = Unique VarName

type instance Select "InParens" 'Shunted = DataConCantHappen

type instance Select "BinaryOperator" 'Shunted = DataConCantHappen

type instance Select "PatternType" 'Shunted = Maybe ShuntedType

type instance Select "VarPat" 'Shunted = Unique LowerAlphaName

type instance Select "ConPat" 'Shunted = Qualified TypeName

type instance Select "TypeApplication" 'Shunted = ShuntedType

-- Selections for 'DeclarationBody'
type instance Select "ValuePatterns" 'Shunted = NoFieldValue

type instance Select "ValueType" 'Shunted = Maybe ShuntedType

type instance Select "ValueTypeDef" 'Shunted = DataConCantHappen

type instance Select "InfixDecl" 'Shunted = DataConCantHappen
type instance Select "KindAnnotation" 'Shunted = NoFieldValue

type instance Select "Alias" 'Shunted = ShuntedType
type instance Select "ADTParam" 'Shunted = ShuntedType

-- Selections for 'Declaration'
type instance Select "DeclarationName" 'Shunted = Qualified Name

-- Selections for 'Type'
type instance Select "TypeKind" 'Shunted = NoFieldValue
type instance Select "TypeVar" 'Shunted = Unique LowerAlphaName

type instance Select "UserDefinedType" 'Shunted = Qualified TypeName

type instance Select "ConstructorName" 'Shunted = Qualified TypeName

type ShuntedExpr = Generic.Expr 'Shunted

type ShuntedExpr' = Generic.Expr' 'Shunted

type ShuntedPattern = Generic.Pattern 'Shunted

type ShuntedPattern' = Generic.Pattern' 'Shunted

type ShuntedBinaryOperator = Generic.BinaryOperator 'Shunted

type ShuntedBinaryOperator' = Generic.BinaryOperator' 'Shunted

type ShuntedType = Generic.Type 'Shunted

type ShuntedType' = Generic.Type' 'Shunted

type ShuntedDeclaration = Generic.Declaration 'Shunted

type ShuntedDeclaration' = Generic.Declaration' 'Shunted

type ShuntedDeclarationBody = Generic.DeclarationBody 'Shunted

type ShuntedDeclarationBody' = Generic.DeclarationBody' 'Shunted

type ShuntedTypeDeclaration = Generic.TypeDeclaration 'Shunted

instance HasDependencies ShuntedDeclaration where
    type Key ShuntedDeclaration = Qualified Name

    keys sd =
        view (_Unwrapped % unlocated % field' @"name" % unlocated) sd :| case sd ^. _Unwrapped % unlocated % field' @"body" % _Unwrapped % unlocated of
            Generic.TypeDeclaration _ (Located _ (Generic.ADT ctors)) _ ->
                toList (NTypeName <<$>> (ctors ^.. each % _1 % unlocated))
            _ -> []
    dependencies decl = case decl ^. _Unwrapped % unlocated % field' @"body" % _Unwrapped % unlocated of
        Generic.Value e NoFieldValue t _ ->
            valueDependencies e
                <> patternDependencies e
                <> (maybeToList t >>= typeDependencies)
        Generic.TypeDeclaration tvs x _ ->
            case x of
                Located _ (Generic.ADT ctors) ->
                    concatMapOf (each % _2 % each) typeDependencies ctors
                Located _ (Generic.Alias t) ->
                    typeDependencies t

valueDependencies :: ShuntedExpr -> [Qualified Name]
valueDependencies =
    concatMapOf (cosmosOn (_Unwrapped % _1 % unlocated)) names
  where
    names :: ShuntedExpr' -> [Qualified Name]
    names (Var (Located _ (Global e))) = NVarName <<$>> [e ^. unlocated]
    names (Constructor (Located _ e)) = [NTypeName <$> e]
    names _ = []

patternDependencies :: ShuntedExpr -> [Qualified Name]
patternDependencies =
    foldOf x
  where
    x = trav % to patternDependencies'
    trav :: Traversal' ShuntedExpr ShuntedPattern
    trav = gplate @ShuntedPattern @ShuntedExpr
    patternDependencies' :: ShuntedPattern -> [Qualified Name]
    patternDependencies' = concatMapOf (cosmosOnOf (_Unwrapped % _1 % unlocated) gplate) names
    names :: ShuntedPattern' -> [Qualified Name]
    names (ConstructorPattern (Located _ e) _) = [NTypeName <$> e]
    names _ = []

typeDependencies :: ShuntedType -> [Qualified Name]
typeDependencies =
    concatMapOf (cosmosOnOf (_Unwrapped % _1 % unlocated) gplate) names
  where
    names :: ShuntedType' -> [Qualified Name]
    names (UserDefinedType (Located _ e)) = [NTypeName <$> e]
    names _ = []
