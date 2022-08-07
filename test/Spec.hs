import ParseTest qualified
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parse test" ParseTest.spec