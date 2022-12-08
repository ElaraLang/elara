module Elara.TypeInfer.Graph where

import Algebra.Graph
import Control.Lens (view)
import Data.Map qualified as Map
import Elara.AST.Frontend (FrontendDecl, ProjectFields (modules))
import Elara.Data.Module (HasDeclarations (..))

-- modulesToGraph :: ProjectFields -> Graph FrontendDecl
-- modulesToGraph fields =
--     let allDecls = foldMap (view declarations) (Map.elems (modules fields))
--      in modulesToGraph' allDecls Prelude.empty
--   where
--     -- modulesToGraph' :: [FrontendDecl] -> Graph FrontendDecl -> Graph FrontendDecl
--     modulesToGraph' [] graph = graph
--     modulesToGraph' (decl : decls) graph = undefined