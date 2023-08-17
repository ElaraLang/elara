{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CUSKs #-}

-- | Typed AST Type without location information. See 'Elara.AST.Frontend.Expr'' for the location information version.
module Elara.AST.Unlocated.Frontend where

import Data.Kind qualified as Kind
import Elara.AST.Generic
import Elara.AST.Select
import Elara.AST.Unlocated (Replace)
import Prelude hiding (Op, group)

type instance ASTLocate' 'UnlocatedFrontend = Unlocated

type instance Select any 'UnlocatedFrontend = Replace 'Frontend 'UnlocatedFrontend (Select any 'Frontend)
