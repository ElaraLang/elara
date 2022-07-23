module Elara.AST.Generic where

import Elara.Data.Located
import Elara.Data.Name (Name)

-- | Class for things that behave roughly like Patterns.
-- This means that even with the different AST pattern types, they can be used in the same way.
class PatternLike pat where
  -- | All the free variables created by a pattern
  -- | For example
  -- | patternNames "let f x = ..." = ["x"]
  patternNames :: pat -> [Name]

patternNameMatches :: PatternLike pat => pat -> Name -> Bool
patternNameMatches pat name = name `elem` patternNames pat -- Maybe this should be a Set
