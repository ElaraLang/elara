import Test.Hspec
import qualified ParserTest

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser Test" ParserTest.spec