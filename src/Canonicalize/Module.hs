{-# LANGUAGE OverloadedRecordDot #-}

module Canonicalize.Module where

import AST.Canonical qualified as Can
import AST.Source qualified as Src
import Canonicalize.Expression qualified as Expr
import Canonicalize.Pattern qualified as Pat
import Canonicalize.Type qualified as Type
import Data.Map qualified as Map
import Elara.ModuleName qualified as ModuleName
import Elara.Name (Name)
import Error.Error qualified as E
import qualified Elara.Package as Pkg

{-
Canonicalizing an AST (term and code structure inspired from https://github.com/elm/compiler/)
is the process of resolving any names in the AST to fully qualified ones, thus removing imports.
This step is essential for making sure that names reference an element that actually exists
-}

canonicalize :: Pkg.Name -> Map.Map ModuleName.Raw Src.Module -> Src.Module -> Either E.Error Can.Module
canonicalize package modulePath module' = do
  let name = ModuleName.Canonical package (Src.getName module')
  let decls = map canonicalizeValue (module'._values)
  return $ Can.Module name decls

canonicalizeValue :: Src.Value -> Can.Def
canonicalizeValue (Src.Value name patterns expr maybeType) = do
  let patterns' = Pat.canonicalize <$> patterns
  let expr' = Expr.canonicalize expr
  let type' = Type.canonicalize <$> maybeType
  case type' of
    Just t -> Can.TypedDef name patterns' expr' t
    Nothing -> Can.Def name patterns' expr'

-- canonicalizeDef :: Src.Def -> Can.Def
-- canonicalizeDef def = do
--   case def of
--     Src.Define name pats exp -> do
--       let pats' = Pat.canonicalize <$> pats
--       let exp' = Expr.canonicalize exp
--       Can.Def name pats' exp'
--     x -> error $ "canonicalizeDef: " ++ show x