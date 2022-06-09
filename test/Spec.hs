import Test.Hspec
import qualified InferTest 

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Infer test" InferTest.spec