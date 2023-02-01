{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Parse.Orphans where

import Data.Char (isLower, isUpper)
import Elara.AST.Name (MaybeQualified (..), Name (NOpName, NTypeName, NVarName), OpName (..), TypeName (..), VarName (..))

instance IsString name => IsString (MaybeQualified name) where
    fromString s = MaybeQualified (fromString s) Nothing

instance IsString VarName where
    fromString = VarName . fromString

instance IsString TypeName where
    fromString = TypeName . fromString

instance IsString OpName where
    fromString = OpName . fromString

instance IsString (Name MaybeQualified) where
    fromString s = case s of
        "" -> error "Empty string is not a valid name"
        c : _ | isUpper c -> NTypeName (fromString s)
        c : _ | isLower c -> NVarName (fromString s)
        _ -> NOpName (fromString s)

