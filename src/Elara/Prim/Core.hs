-- Primitive core names
module Elara.Prim.Core where

import Effectful
import Elara.AST.Name (ModuleName (..), NameLike (nameText), Qualified (..))
import Elara.AST.VarRef
import Elara.Core (DataCon (..), TyCon (..), TyConDetails (..), Type (..), TypeVariable (TypeVariable), Var (..))
import Elara.Data.Kind (ElaraKind (TypeKind))
import Elara.Data.Unique.Effect
import Elara.Prim (charName, doubleName, floatName, intName, ioName, mkPrimQual, stringName, unitName)

trueCtorName :: Qualified Text
trueCtorName = Qualified "True" (ModuleName ("Elara" :| ["Prim"]))

falseCtorName :: Qualified Text
falseCtorName = Qualified "False" (ModuleName ("Elara" :| ["Prim"]))

emptyListCtorName :: IsString s => Qualified s
emptyListCtorName = Qualified "Nil" (ModuleName ("Elara" :| ["Prim"]))

consCtorName :: IsString s => Qualified s
consCtorName = Qualified "Cons" (ModuleName ("Elara" :| ["Prim"]))

tuple2CtorName :: Qualified Text
tuple2CtorName = Qualified "Tuple2" (ModuleName ("Elara" :| ["Prim"]))

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
boolCon = TyCon (mkPrimQual "Bool") (TyADT [trueCtorName, falseCtorName])

intCon :: TyCon
intCon = TyCon (mkPrimQual $ nameText intName) Prim

floatCon :: TyCon
floatCon = TyCon (mkPrimQual $ nameText floatName) Prim

stringCon :: TyCon
stringCon = TyCon (mkPrimQual $ nameText stringName) Prim

charCon :: TyCon
charCon = TyCon (mkPrimQual $ nameText charName) Prim

doubleCon :: TyCon
doubleCon = TyCon (mkPrimQual $ nameText doubleName) Prim

ioCon :: TyCon
ioCon = TyCon (mkPrimQual $ nameText ioName) Prim

unitCon :: TyCon
unitCon = TyCon (mkPrimQual $ nameText unitName) Prim

trueCtor :: DataCon
trueCtor = DataCon trueCtorName (ConTy boolCon) boolCon

falseCtor :: DataCon
falseCtor = DataCon falseCtorName (ConTy boolCon) boolCon

unitCtor :: DataCon
unitCtor = DataCon unitConName (ConTy unitCon) unitCon

unitConName :: Qualified Text
unitConName = mkPrimQual "()"

undefinedId :: UniqueGen :> r => Eff r Var
undefinedId = do
    a <- makeUnique (Just "a")
    let tvA = TypeVariable a TypeKind
    pure $ Id (Global $ mkPrimQual "undefined") (ForAllTy tvA (TyVarTy tvA)) Nothing
