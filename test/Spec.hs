import Parse qualified
import Test.Hspec
import AST.Module.Inspection qualified as Inspection 

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do
  describe "AST inspection test" Inspection.spec
  describe "Parse test" Parse.spec