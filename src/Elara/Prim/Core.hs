-- Primitive core names
module Elara.Prim.Core where

import Elara.AST.Name (ModuleName (..), Qualified (..))
import Elara.AST.VarRef
import Elara.Core (DataCon (..), TyCon (..), TyConDetails (..), Type (..), TypeVariable (TypeVariable), Var (..))
import Elara.Data.Kind (ElaraKind (TypeKind))
import Elara.Data.Unique
import Elara.Prim (mkPrimQual)
import Polysemy

trueCtorName :: Qualified Text
trueCtorName = Qualified "True" (ModuleName ("Elara" :| ["Prim"]))

falseCtorName :: Qualified Text
falseCtorName = Qualified "False" (ModuleName ("Elara" :| ["Prim"]))

emptyListCtorName :: IsString s => Qualified s
emptyListCtorName = Qualified "Nil" (ModuleName ("Elara" :| ["Prim"]))

consCtorName :: IsString s => Qualified s
consCtorName = Qualified "Cons" (ModuleName ("Elara" :| ["Prim"]))

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
    pure $ Id (Global $ Identity $ mkPrimQual "undefined") (ForAllTy tvA (TyVarTy tvA)) Nothing
