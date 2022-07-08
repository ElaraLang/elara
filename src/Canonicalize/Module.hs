{-# LANGUAGE OverloadedRecordDot #-}

module Canonicalize.Module where

import AST.Canonical qualified as Can
import AST.Source qualified as Src
import Canonicalize.Expression qualified as Expr
import Canonicalize.Pattern qualified as Pat
import Canonicalize.Type qualified as Type
import Data.Map qualified as Map
import Elara.ModuleName qualified as ModuleName
import Elara.Package qualified as Pkg
import Error.Error qualified as E

{-
Canonicalizing an AST (term and code structure inspired from https://github.com/elm/compiler/)
is the process of resolving any names in the AST to fully qualified ones, thus removing imports.
This step is essential for making sure that names reference an element that actually exists
-}

canonicalize :: Pkg.Name -> Map.Map ModuleName.Raw Src.Module -> Src.Module -> Either E.Error Can.Module
canonicalize package _ module' = do
  let name = ModuleName.Canonical package (Src.getName module')
  let defs = canonicalizeDecl <$> module'._decls
  let decls = map canonicalizeValue (module'._values)
  return $ Can.Module name (defs ++ decls)

canonicalizeValue :: Src.Value -> Can.Def
canonicalizeValue (Src.Value name patterns expr) = do
  let patterns' = Pat.canonicalize <$> patterns
  let expr' = Expr.canonicalize expr
  Can.Def name patterns' expr'

canonicalizeDecl :: Src.Decl -> Can.Def
canonicalizeDecl (Src.Decl name type') = Can.TypedDef name (Type.canonicalize type')
