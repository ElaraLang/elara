{-# LANGUAGE OverloadedRecordDot #-}

module Canonicalize.Module where

import AST.Canonical qualified as Can
import AST.Source qualified as AST
import AST.Source qualified as Src
import Data.Map qualified as Map
import Elara.ModuleName qualified as ModuleName
import Elara.Name (Name)
import Error.Error qualified as E
import qualified Canonicalize.Pattern as Pat

{-
Canonicalizing an AST (term and code structure inspired from https://github.com/elm/compiler/)
is the process of resolving any names in the AST to fully qualified ones, thus removing imports.
This step is essential for making sure that names reference an element that actually exists
-}

canonicalize :: Name -> Map.Map ModuleName.Raw AST.Module -> AST.Module -> Either E.Error Can.Module
canonicalize package modulePath module' = do
  let name = ModuleName.Canonical package (Src.getName module')
  let decls = map canonicalizeDef (module'._values)
  return $ Can.Module name decls

canonicalizeValue :: AST.Value -> Can.Def
canonicalizeValue (AST.Value name patterns expr maybeType) = do
  let patterns' = Pat.canonicalize <$> pattern
  let expr' = Expr.canonicalize expr
  let type' = Type.canonicalize <$> maybeType
  case type' of
    Just t -> Can.TypedDef name patterns' expr' t
    Nothing -> Can.Def name patterns' expr'