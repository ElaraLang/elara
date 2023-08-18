{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Select where

-- type LocatedAST :: AST
data LocatedAST = Frontend | Desugared | Renamed | Shunted | Typed | Core

-- type UnlocatedAST :: AST
data UnlocatedAST = UnlocatedFrontend | UnlocatedDesugared | UnlocatedRenamed | UnlocatedShunted | UnlocatedTyped
