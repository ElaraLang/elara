{-# LANGUAGE UndecidableInstances #-}

-- | Typed AST Type without location information. See 'Elara.AST.Typed.Expr'' for the location information version.
module Elara.AST.Unlocated.Typed where

import Elara.AST.Generic
import Elara.AST.Select
import Prelude hiding (Op, group)

type instance ASTLocate' 'UnlocatedTyped = Unlocated
type instance Select any 'UnlocatedTyped = Select any 'Typed