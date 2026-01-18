{-# LANGUAGE OverloadedLists #-}

{- | Stores information about the primitive functions of Elara. These are still written in the source code, with a special name and value.
The compiler will then replace these with the actual primitive functions.
-}
module Elara.Prim where

import Elara.AST.Name (MaybeQualified (..), ModuleName (..), Qualified (..), TypeName (..), VarName (..), VarOrConName (..))
import Elara.AST.Region (Located, SourceRegion, generatedLocated, generatedSourceRegion)
import Elara.AST.VarRef (VarRef, VarRef' (Global))
import Elara.Data.Kind (ElaraKind (..))

consName :: TypeName
consName = "::"

cons :: MaybeQualified VarOrConName
cons = MaybeQualified (ConName "Cons") (Just primModuleName)

fetchPrimitiveName :: VarName
fetchPrimitiveName = NormalVarName "elaraPrimitive"

fetchPrimitive :: (VarRef VarName)
fetchPrimitive = Global (mkPrimVarRef fetchPrimitiveName)

stringName :: TypeName
stringName = TypeName "Prim_String"

intName :: TypeName
intName = TypeName "Prim_Int"

floatName :: TypeName
floatName = TypeName "Prim_Float"

unitName :: TypeName
unitName = TypeName "()"

boolName :: TypeName
boolName = TypeName "Bool"

trueName :: TypeName
trueName = TypeName "True"

falseName :: TypeName
falseName = TypeName "False"

charName :: TypeName
charName = TypeName "Prim_Char"

doubleName :: TypeName
doubleName = TypeName "Prim_Double"

ioName :: TypeName
ioName = TypeName "Prim_IO"

fullListName :: IsString a => Qualified a
fullListName = mkPrimQual "List"

primModuleName :: ModuleName
primModuleName = ModuleName ["Elara", "Prim"]

primRegion :: SourceRegion
primRegion = generatedSourceRegion (Just "<primitive>")

primLocated :: a -> Located a
primLocated = generatedLocated (Just "<primitive>")

mkPrimQual :: c -> Qualified c
mkPrimQual c = Qualified c primModuleName

mkPrimVarRef :: c -> Located (Qualified c)
mkPrimVarRef c = generatedLocated (Just "<primitive>") (mkPrimQual c)

primitiveVars :: [VarName]
primitiveVars = [fetchPrimitiveName]

primitiveTypes :: [TypeName]
primitiveTypes = [stringName, charName, intName, floatName, unitName, consName]

fixityAnnotationName :: Qualified TypeName
fixityAnnotationName = mkPrimQual (TypeName "Fixity")

associativityAnnotationName :: Qualified TypeName
associativityAnnotationName = mkPrimQual (TypeName "Associativity")

leftAssociativeAnnotationName :: Qualified TypeName
leftAssociativeAnnotationName = mkPrimQual (TypeName "LeftAssociative")
rightAssociativeAnnotationName :: Qualified TypeName
rightAssociativeAnnotationName = mkPrimQual (TypeName "RightAssociative")
nonAssociativeAnnotationName :: Qualified TypeName
nonAssociativeAnnotationName = mkPrimQual (TypeName "NonAssociative")

primKindCheckContext :: Map (Qualified TypeName) ElaraKind
primKindCheckContext =
    -- assume all primitive types are kind Type
    fromList ((\x -> (Qualified x primModuleName, TypeKind)) <$> primitiveTypes)
        <> fromList
            [ (Qualified ioName primModuleName, FunctionKind TypeKind TypeKind)
            ] -- Except for IO which is kind Type -> Type
