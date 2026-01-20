-- | Test region and location utilities
module Region (
    testRegion,
    testLocated,
    testModuleName,
    qualifiedTest,
) where

import Elara.AST.Name (ModuleName (..), Qualified (..))
import Elara.AST.Region (Located (..), SourceRegion, generatedSourceRegion)

--------------------------------------------------------------------------------
-- Test Region Utilities
--------------------------------------------------------------------------------

testRegion :: SourceRegion
testRegion = generatedSourceRegion (Just "<testing>")

testLocated :: a -> Located a
testLocated = Located testRegion

testModuleName :: ModuleName
testModuleName = ModuleName ("Testing" :| [])

qualifiedTest :: a -> Qualified a
qualifiedTest = flip Qualified testModuleName
