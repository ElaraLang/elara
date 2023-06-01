{-# LANGUAGE OverloadedLists #-}

{- | Stores information about the primitive functions of Elara. These are still written in the source code, with a special name and value.
 The compiler will then replace these with the actual primitive functions.
-}
module Elara.Prim where

import Elara.AST.Name (ModuleName (..), Name (NVarName), Qualified (..), VarName (NormalVarName))
import Elara.AST.Region (Located, generatedLocated, generatedSourceRegion)
import Elara.AST.VarRef (VarRef (..), VarRef' (Global))
import Elara.Rename (RenameState (RenameState))

fetchPrimitiveName :: VarName
fetchPrimitiveName = NormalVarName "elaraPrimitive"

primModuleName :: ModuleName
primModuleName = ModuleName ["Elara", "Prim"]

mkPrimVarRef :: c -> Located (Qualified c)
mkPrimVarRef c = generatedLocated (Just "<primitive>") (Qualified c primModuleName)

primitiveRenameState :: RenameState
primitiveRenameState =
    let vars =
            fromList
                [ (fetchPrimitiveName, Global (mkPrimVarRef fetchPrimitiveName))
                ] ::
                Map VarName (VarRef VarName)
     in RenameState vars mempty mempty