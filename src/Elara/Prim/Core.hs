-- Primitive core names
module Elara.Prim.Core where

import Effectful
import Elara.AST.Name (NameLike (nameText), Qualified (..))
import Elara.AST.VarRef
import Elara.Core (DataCon (..), TyCon (..), TyConDetails (..), Type (..), TypeVariable (TypeVariable), Var (..))
import Elara.Data.Kind (ElaraKind (TypeKind))
import Elara.Data.Unique.Effect
import Elara.Prim (KnownType (..), KnownTypeInfo (..), OpaquePrim (..), WiredInPrim (..), falseCtorName, knownTypeInfo, mkPrimQual, trueCtorName, wiredInPrimCtors)

-- | Build a 'TyCon' for a wired-in primitive, deriving constructors from the registry
wiredInTyCon :: WiredInPrim -> TyCon
wiredInTyCon p =
    let info = knownTypeInfo (KnownWiredIn p)
        ctors = wiredInPrimCtors p
     in TyCon (nameText <$> knownQualified info) (TyADT ctors)

boolCon :: TyCon
boolCon = wiredInTyCon WiredInBool

listCon :: TyCon
listCon = wiredInTyCon WiredInList

tuple2Con :: TyCon
tuple2Con = wiredInTyCon WiredInTuple2

orderingCon :: TyCon
orderingCon = wiredInTyCon WiredInOrdering

unitCon :: TyCon
unitCon = wiredInTyCon WiredInUnit

-- | Build a 'TyCon' for an opaque primitive.
opaquePrimTyCon :: OpaquePrim -> TyCon
opaquePrimTyCon p = TyCon (nameText <$> knownQualified (knownTypeInfo (KnownOpaque p))) (Prim p)

intCon :: TyCon
intCon = opaquePrimTyCon PrimInt

floatCon :: TyCon
floatCon = opaquePrimTyCon PrimFloat

stringCon :: TyCon
stringCon = opaquePrimTyCon PrimString

charCon :: TyCon
charCon = opaquePrimTyCon PrimChar

doubleCon :: TyCon
doubleCon = opaquePrimTyCon PrimDouble

ioCon :: TyCon
ioCon = opaquePrimTyCon PrimIO

trueCtor :: DataCon
trueCtor = DataCon trueCtorName (ConTy boolCon) boolCon

falseCtor :: DataCon
falseCtor = DataCon falseCtorName (ConTy boolCon) boolCon

unitConName :: Qualified Text
unitConName = mkPrimQual "Unit"

unitCtor :: DataCon
unitCtor = DataCon unitConName (ConTy unitCon) unitCon

undefinedId :: UniqueGen :> r => Eff r Var
undefinedId = do
    a <- makeUnique (Just "a")
    let tvA = TypeVariable a TypeKind
    pure $ Id (Global $ mkPrimQual "undefined") (ForAllTy tvA (TyVarTy tvA)) Nothing
