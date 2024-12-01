module Region where

import Elara.AST.Name (ModuleName (..), Qualified (..))
import Elara.AST.Region

testRegion :: SourceRegion
testRegion = generatedSourceRegion (Just "<testing>")

testLocated :: a -> Located a
testLocated = Located testRegion

testModuleName :: ModuleName
testModuleName = ModuleName ("Testing" :| [])

qualifiedTest :: a -> Qualified a
qualifiedTest = flip Qualified testModuleName
