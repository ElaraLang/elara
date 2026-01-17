{-# LANGUAGE OverloadedLists #-}

module Elara.Prim.Rename where

import Elara.AST.VarRef (VarRef' (Global))
import Elara.Prim (ioName, mkPrimVarRef, primitiveTypes)
import Elara.Rename.State

-- | The renaming state containing all primitive types and values.
primitiveRenameState :: RenameState
primitiveRenameState =
    let vars =
            []
        types =
            fromList ((\x -> (x, one $ Global (mkPrimVarRef x))) <$> primitiveTypes)
                <> fromList [(ioName, one $ Global (mkPrimVarRef ioName))]
     in RenameState vars types mempty
