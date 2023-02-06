import Parse qualified
import Parse.Expression (ppEq)
import Test.Hspec
import Test.QuickCheck (verboseCheck)

main :: IO ()
main = do
  verboseCheck ppEq
  hspec spec

spec :: Spec
spec = do
  describe "Parse test" Parse.spec