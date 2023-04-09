{- | Performs kind inference on a type.


>> infer "type alias Identity a = a"
"k -> k"

>> infer "type Maybe a = Either a Unit"
Type -> Type

>> infer "type Either a b = Left a | Right b"
Type -> Type -> Type

>> infer "type Proxy a = Proxy"
k -> Type
-}
module Elara.Data.Kind.Infer where

import Control.Lens ((^.))
import Elara.AST.Name (LowerAlphaName)
import Elara.AST.Region (Located, unlocated)
import Elara.AST.Renamed qualified as AST
import Elara.AST.Shunted qualified as AST
import Elara.Data.Kind
import Elara.Data.Unique (Unique)
import Polysemy (Sem)

type TypeVar = Unique LowerAlphaName

type KindConstraint = (TypeVar, ElaraKind)



inferKind :: [Located (Unique LowerAlphaName)] -> AST.TypeDeclaration -> Sem r ElaraKind
inferKind [] (AST.Alias t) = inferTypeKind (t ^. unlocated)

inferTypeKind :: AST.Type -> Sem r ElaraKind
inferTypeKind (AST.TypeVar v) = pure TypeKind