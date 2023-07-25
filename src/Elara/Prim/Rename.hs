{-# LANGUAGE OverloadedLists #-}

module Elara.Prim.Rename where

import Elara.AST.Name (TypeName)
import Elara.AST.VarRef (VarRef' (Global))
import Elara.Prim (ioName, mkPrimVarRef, primitiveTypes, primitiveVars)
import Elara.Rename (RenameState (RenameState))

primitiveRenameState :: RenameState
primitiveRenameState =
    let vars =
            fromList ((\x -> (x, Global (mkPrimVarRef x))) <$> primitiveVars)
        types =
            fromList ((\x -> (x, Global (mkPrimVarRef x))) <$> primitiveTypes) <> fromList [(ioName, Global (mkPrimVarRef ioName))]
     in RenameState vars types mempty