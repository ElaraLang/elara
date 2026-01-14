{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeAbstractions #-}

-- | Defines useful classes and functions for introspecting ASTs
module Elara.AST.Introspection (IntrospectableAnnotations (..)) where

import Elara.AST.Generic.Types
import Elara.AST.Renamed ()
import Elara.AST.Select

-- | Value-level proof that an AST's annotations can be coerced to a specific type over all selectors
class IntrospectableAnnotations ast where
    -- | Get all annotations for a given selector in the AST
    getAnnotations :: forall sel. Select (Annotations sel) ast -> [Annotation ast]

instance IntrospectableAnnotations Renamed where
    getAnnotations a = a
