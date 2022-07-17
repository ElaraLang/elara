module Elara.AST.Frontend where

import Data.Map qualified as M
import Elara.Data.Located (Located)
import Elara.Data.Module
import Elara.Data.Name (ModuleName, Name)
import Elara.Data.TypeAnnotation (TypeAnnotation)

{- Least abstract AST, closest to elara source code.
Things like comments are preserved
 -}

-- Information about the whole project
data ProjectFields = ProjectFields
  { modules :: M.Map ModuleName (Module LocatedExpr TypeAnnotation (Maybe ModuleName))
  }

type LocatedExpr = Located Expr

data Expr
  = Int Int
  | Float Double
  | Char Char
  | String String
  | Bool Bool -- Do we want Bool has a primitive or a constructor?
  | Var Name
  | Constructor Name
  | Lambda {arguments :: [Name], body :: LocatedExpr}
  | FunctionCall {function :: LocatedExpr, argument :: LocatedExpr}
  | BinaryOperator {operator :: LocatedExpr, left :: LocatedExpr, right :: LocatedExpr}
  | If {condition :: LocatedExpr, then_ :: LocatedExpr, else_ :: LocatedExpr}
  | Unit
  deriving (Show)