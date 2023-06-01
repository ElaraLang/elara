{-# LANGUAGE OverloadedLists #-}

{- | Stores information about the primitive functions of Elara. These are still written in the source code, with a special name and value.
 The compiler will then replace these with the actual primitive functions.
-}
module Elara.Prim where

import Elara.AST.Name (ModuleName (..), Name (NVarName), Qualified (..), VarName (NormalVarName))
import Elara.AST.Region (IgnoreLocation (IgnoreLocation), Located, SourceRegion, generatedLocated, generatedSourceRegion)
import Elara.AST.VarRef (VarRef (..), VarRef' (Global))
import Elara.Rename (RenameState (RenameState))
import Elara.TypeInfer.Context (Context, Entry (Annotation))
import Elara.TypeInfer.Domain (Domain (..))
import Elara.TypeInfer.Type (Type (..))

fetchPrimitiveName :: VarName
fetchPrimitiveName = NormalVarName "elaraPrimitive"

primModuleName :: ModuleName
primModuleName = ModuleName ["Elara", "Prim"]

primRegion :: SourceRegion
primRegion = generatedSourceRegion (Just "<primitive>")

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

primitiveTCContext :: Context SourceRegion
primitiveTCContext =
    [ Annotation
        (Global (IgnoreLocation $ mkPrimVarRef (NVarName fetchPrimitiveName)))
        (Forall primRegion primRegion "a" Type (VariableType primRegion "a")) -- elaraPrimitive :: forall a. a
    ]