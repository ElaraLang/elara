import Test.Hspec
import qualified ParserTest
import qualified InferTest 

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser Test" ParserTest.spec
  describe "Infer test" InferTest.spec