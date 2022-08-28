module Elara.Error where

import Elara.AST.Frontend (LocatedExpr)
import Elara.Data.Name (ModuleName, Name)
import Text.Megaparsec (ShowErrorComponent (showErrorComponent))

data Error
  = GenericError Text
  | DesugarError DesugarError
  deriving (Ord, Eq, Show)

instance ShowErrorComponent Error where
  showErrorComponent = show

data DesugarError
  = UnknownVarName
      { var :: Name,
        insideModule :: ModuleName
      }
  | CantDesugar Text
  | AmbiguousReference
      { name :: Name,
        insideModule :: ModuleName,
        candidates :: [ModuleName]
      }
  | MultipleDeclarations
      { name :: Name,
        insideModule :: ModuleName
      }
  | EmptyBlock
      { boundTo :: Name,
        insideModule :: ModuleName
      }
  | LetEndsBlock
      { boundTo :: Name,
        insideModule :: ModuleName,
        expr :: LocatedExpr
      }
  deriving (Ord, Eq, Show)