module Elara.Error where

import Data.Text (Text)
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
  deriving (Ord, Eq, Show)