{-# LANGUAGE UndecidableInstances #-}

-- | Like 'Elara.AST.Shunted' but post-kind inference so the kinds of types are always known
module Elara.AST.Kinded where

import Elara.AST.Generic (ASTLocate', ASTQual, Select)
import Elara.AST.Generic qualified as Generic
import Elara.AST.Name (Qualified)
import Elara.AST.Region (Located (..))
import Elara.AST.Select (LocatedAST (Kinded, MidKinded, Shunted))
import Elara.Data.Kind (ElaraKind)
import Elara.Data.Unique (UniqueId)

type instance ASTLocate' 'Kinded = Located
type instance ASTLocate' 'MidKinded = Located

type instance ASTQual 'Kinded = Qualified
type instance ASTQual 'MidKinded = Qualified

type instance Select x 'Kinded = ReplaceKinded x (Select x 'MidKinded)
type instance Select x 'MidKinded = ReplaceMidKinded x (Select x 'Shunted)

type family ReplaceKinded x r where
    ReplaceKinded "KindAnnotation" r = ElaraKind
    ReplaceKinded "TypeKind" r = ElaraKind
    ReplaceKinded "ADTParam" r = KindedType
    ReplaceKinded "Alias" r = KindedType
    ReplaceKinded x r = r

type family ReplaceMidKinded x r where
    ReplaceMidKinded "TypeKind" r = UniqueId
    ReplaceMidKinded "ADTParam" r = MidKindedType
    ReplaceMidKinded x r = r

type KindedTypeDeclaration = Generic.TypeDeclaration 'Kinded
type MidKindedTypeDeclaration = Generic.TypeDeclaration 'MidKinded
type KindedType = Generic.Type 'Kinded
type KindedType' = Generic.Type' 'Kinded
type MidKindedType = Generic.Type 'MidKinded
