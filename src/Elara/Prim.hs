{-# LANGUAGE OverloadedLists #-}

{- | Stores information about the primitive functions of Elara. These are still written in the source code, with a special name and value.
The compiler will then replace these with the actual primitive functions.
-}
module Elara.Prim where

import Elara.AST.Name (MaybeQualified (..), ModuleName (..), Name (..), Qualified (..), TypeName (..), VarName (..), VarOrConName (..))
import Elara.AST.Region (Located, SourceRegion, generatedLocated, generatedSourceRegion)
import Elara.AST.VarRef (VarRef, VarRef' (Global), ignoreLocation)
import Elara.Data.Kind (ElaraKind (..))
import Elara.Shunt

consName :: TypeName
consName = "::"

cons :: MaybeQualified VarOrConName
cons = MaybeQualified (ConName "Cons") (Just primModuleName)

fetchPrimitiveName :: VarName
fetchPrimitiveName = NormalVarName "elaraPrimitive"

fetchPrimitive :: (VarRef VarName)
fetchPrimitive = Global (mkPrimVarRef fetchPrimitiveName)

stringName :: TypeName
stringName = TypeName "String"

intName :: TypeName
intName = TypeName "Int"

boolName :: TypeName
boolName = TypeName "Bool"

trueName :: TypeName
trueName = TypeName "True"

falseName :: TypeName
falseName = TypeName "False"

charName :: TypeName
charName = TypeName "Char"

ioName :: TypeName
ioName = TypeName "IO"

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
primitiveTypes = [stringName, charName, intName, boolName, trueName, falseName, consName]

primKindCheckContext :: Map (Qualified TypeName) ElaraKind
primKindCheckContext =
    -- assume all primitive types are kind Type
    fromList ((\x -> (Qualified x primModuleName, TypeKind)) <$> primitiveTypes)
        <> fromList [(Qualified ioName primModuleName, FunctionKind TypeKind TypeKind)] -- Except for IO which is kind Type -> Type

primOpTable :: OpTable
primOpTable =
    fromList
        [ (ignoreLocation $ Global (mkPrimVarRef $ NVarName $ OperatorVarName "|>"), OpInfo (mkPrecedence 0) LeftAssociative)
        , (ignoreLocation $ Global (mkPrimVarRef $ NVarName $ OperatorVarName "+"), OpInfo (mkPrecedence 6) LeftAssociative)
        , (ignoreLocation $ Global (mkPrimVarRef $ NVarName $ OperatorVarName "-"), OpInfo (mkPrecedence 6) LeftAssociative)
        , (ignoreLocation $ Global (mkPrimVarRef $ NVarName $ OperatorVarName "*"), OpInfo (mkPrecedence 7) LeftAssociative)
        , (ignoreLocation $ Global (mkPrimVarRef $ NVarName $ OperatorVarName "/"), OpInfo (mkPrecedence 7) LeftAssociative)
        ]