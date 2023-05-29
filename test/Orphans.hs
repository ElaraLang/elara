{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans where

import Data.Char (isLower, isUpper)
import Data.Text (splitOn)

import Elara.AST.Name (MaybeQualified (..), ModuleName (..), Name (NOpName, NTypeName, NVarName), OpName (..), TypeName (..), Unqualified (..), VarName (..))

instance IsString VarName where
    fromString = NormalVarName . fromString

instance IsString Name where
    fromString s = case s of
        "" -> error "Empty string is not a valid name"
        c : _ | isUpper c -> NTypeName (fromString s)
        c : _ | isLower c -> NVarName (fromString s)
        _ -> NOpName (fromString s)

instance IsString s => IsString (MaybeQualified s) where
    fromString s = MaybeQualified (fromString s) Nothing

instance IsString s => IsString (Unqualified s) where
    fromString s = Unqualified (fromString s)

instance IsString ModuleName where
    -- oh boy i love having 2 string types
    fromString s = ModuleName $ fromList (fromString . toString <$> splitOn (fromString ".") (fromString s))
