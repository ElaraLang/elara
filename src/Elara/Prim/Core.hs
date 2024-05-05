-- Primitive core names
module Elara.Prim.Core where

import Elara.AST.Name (ModuleName (..), Qualified (..))
import Elara.Core (DataCon (..), TyCon (..), TyConDetails (..), Type (..), TypeVariable (TypeVariable), Var (..))
import Elara.Data.Kind (ElaraKind (TypeKind))
import Elara.Data.Unique
import Elara.Prim (mkPrimQual)
import Polysemy
import Elara.AST.VarRef

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
    pure $ ForAllTy tv (FuncTy (TyVarTy tv) (FuncTy (AppTy (ConTy listCon) (TyVarTy tv)) (AppTy (ConTy listCon) (TyVarTy tv))))

-- tuple2CtorName :: Qualified Text
-- tuple2CtorName = Qualified "Tuple2" (ModuleName ("Elara" :| ["Prim"]))

-- tuple2CtorType :: Member UniqueGen r => Sem r Type
-- tuple2CtorType = do
--     a <- makeUnique (Just "a")
--     b <- makeUnique (Just "b")
--     let tvA = TypeVariable a TypeKind
--     let tvB = TypeVariable b TypeKind
--     pure $ ForAllTy tvA (ForAllTy tvB (FuncTy (TyVarTy tvA) (FuncTy (TyVarTy tvB) tuple2Con)))

fetchPrimitiveName :: Qualified Text
fetchPrimitiveName = Qualified "elaraPrimitive" (ModuleName ("Elara" :| ["Prim"]))

boolCon :: TyCon
boolCon = TyCon (mkPrimQual "Bool") Prim

listCon :: TyCon
listCon = TyCon (mkPrimQual "[]") Prim

intCon :: TyCon
intCon = TyCon (mkPrimQual "Int") Prim

stringCon :: TyCon
stringCon = TyCon (mkPrimQual "String") Prim

unitCon :: TyCon
unitCon = TyCon (mkPrimQual "()") Prim

charCon :: TyCon
charCon = TyCon (mkPrimQual "Char") Prim

ioCon :: TyCon
ioCon = TyCon (mkPrimQual "IO") Prim

trueCtor :: DataCon
trueCtor = DataCon trueCtorName (ConTy boolCon) boolCon

falseCtor :: DataCon
falseCtor = DataCon falseCtorName (ConTy boolCon) boolCon

undefinedId :: Member UniqueGen r => Sem r Var
undefinedId = do
    a <- makeUnique (Just "a")
    let tvA = TypeVariable a TypeKind
    pure $ Id (Global $ Identity $ mkPrimQual "undefined") (ForAllTy tvA (TyVarTy tvA) ) Nothing 