{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans where

import Data.Char (isLower, isUpper)
import Data.Text (splitOn)

import Elara.AST.Name (MaybeQualified (..), ModuleName (..), Name (NOpName, NTypeName, NVarName), OpName (..), TypeName (..), VarName (..))

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

instance IsString ModuleName where
    -- oh boy i love having 2 string types
    fromString s = ModuleName $ fromList (fromString . toString <$> splitOn (fromString ".") (fromString s))