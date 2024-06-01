{-# LANGUAGE OverloadedLists #-}

module Elara.Prim.Rename where

import Elara.AST.VarRef (VarRef' (Global))
import Elara.Prim (ioName, mkPrimVarRef, primitiveTypes, primitiveVars)
import Elara.Rename (RenameState (RenameState))

primitiveRenameState :: RenameState
primitiveRenameState =
    let vars =
            fromList ((\x -> (x, one $ Global (mkPrimVarRef x))) <$> primitiveVars)
        types =
            fromList ((\x -> (x, one $ Global (mkPrimVarRef x))) <$> primitiveTypes)
                <> fromList [(ioName, one $ Global (mkPrimVarRef ioName))]
     in RenameState vars types mempty
