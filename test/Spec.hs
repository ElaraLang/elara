import Infer qualified
import Lex qualified
import Parse qualified
import Shunt qualified
import Test.Hspec

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = parallel $ do
  describe "Lexing test" Lex.spec
  describe "Parsing Test" Parse.spec
  describe "Infer Test" Infer.spec
  describe "Shunt Test" Shunt.spec
