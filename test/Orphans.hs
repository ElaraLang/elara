{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans where

import Data.Char (isLower, isUpper)
import Data.Text (splitOn)
import Elara.AST.Name (MaybeQualified (..), ModuleName (..), Name (NameType, NameValue), Unqualified (..))

instance IsString Name where
    fromString s = case s of
        "" -> error "Empty string is not a valid name"
        c : _ | isUpper c -> NameType (fromString s)
        c : _ | isLower c -> NameValue (fromString s)
        _ -> error "Invalid name"

instance IsString s => IsString (MaybeQualified s) where
    fromString s = MaybeQualified (fromString s) Nothing

instance IsString s => IsString (Unqualified s) where
    fromString s = Unqualified (fromString s)

instance IsString ModuleName where
    fromString s = ModuleName $ fromList (splitOn "." (fromString s))
