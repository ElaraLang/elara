module Parse where

import Arbitrary.AST (genExpr)
import Elara.AST.Generic
import Elara.AST.Select
import Elara.AST.StripLocation
import Elara.Parse.Expression (exprParser)
import Hedgehog
import Orphans ()
import Parse.Common
import Parse.Expressions qualified as Expressions
import Parse.Patterns qualified as Patterns
import Print (showPrettyUnannotated)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = parallel $ do
  Patterns.spec
  Expressions.spec

-- ppEq :: Property
-- ppEq = do
--     expr <- removeInParens <$> forAll
--     let source = showPrettyUnannotated $ pretty expr
--         parsed = run $ runError $ lexAndParse exprParser source
--         cleaned = removeInParens . stripExprLocation <$> parsed
--      in counterexample ("pretty source: \n" <> toString (showPretty $ pretty source)) (cleaned `shouldParseProp` expr)
