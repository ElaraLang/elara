-- Primitive core names
module Elara.Prim.Core where

import Elara.AST.Name (ModuleName (..), Qualified (..))
import Elara.Core (DataCon (..), Type (..), TypeVariable (TypeVariable))
import Elara.Data.Kind (ElaraKind (TypeKind))
import Elara.Data.Unique
import Elara.Prim (mkPrimQual)
import Polysemy

trueCtorName :: Qualified Text
trueCtorName = Qualified "True" (ModuleName ("Elara" :| ["Prim"]))

falseCtorName :: Qualified Text
falseCtorName = Qualified "False" (ModuleName ("Elara" :| ["Prim"]))

emptyListCtorName :: Qualified Text
emptyListCtorName = Qualified "empty" (ModuleName ("Elara" :| ["Prim"]))

consCtorName :: Qualified Text
consCtorName = Qualified "cons" (ModuleName ("Elara" :| ["Prim"]))

consType :: Member UniqueGen r => Sem r Type
consType = do
    a <- makeUnique (Just "a")
    let tv = TypeVariable a TypeKind
    pure $ ForAllTy tv (FuncTy (TyVarTy tv) (FuncTy (AppTy listCon (TyVarTy tv)) (AppTy listCon (TyVarTy tv))))

tuple2CtorName :: Qualified Text
tuple2CtorName = Qualified "Tuple2" (ModuleName ("Elara" :| ["Prim"]))

tuple2CtorType :: Member UniqueGen r => Sem r Type
tuple2CtorType = do
    a <- makeUnique (Just "a")
    b <- makeUnique (Just "b")
    let tvA = TypeVariable a TypeKind
    let tvB = TypeVariable b TypeKind
    pure $ ForAllTy tvA (ForAllTy tvB (FuncTy (TyVarTy tvA) (FuncTy (TyVarTy tvB) tuple2Con)))

fetchPrimitiveName :: Qualified Text
fetchPrimitiveName = Qualified "elaraPrimitive" (ModuleName ("Elara" :| ["Prim"]))

boolCon :: Type
boolCon = ConTy (mkPrimQual "Bool")

listCon :: Type
listCon = ConTy (mkPrimQual "[]")

intCon :: Type
intCon = ConTy (mkPrimQual "Int")

stringCon :: Type
stringCon = ConTy (mkPrimQual "String")

unitCon :: Type
unitCon = ConTy (mkPrimQual "()")

charCon :: Type
charCon = ConTy (mkPrimQual "Char")

ioCon :: Type
ioCon = ConTy (mkPrimQual "IO")

trueCtor :: DataCon
trueCtor = DataCon trueCtorName boolCon boolCon

falseCtor :: DataCon
falseCtor = DataCon falseCtorName boolCon boolCon

tuple2Con :: Type
tuple2Con = ConTy (mkPrimQual "(,)")

tuple2Ctor :: DataCon
tuple2Ctor = DataCon tuple2CtorName _ tuple2Con
