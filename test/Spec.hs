import Lex qualified
import Parse qualified
import Infer qualified
import Test.Hspec

main :: IO ()
main = do
    hspec spec

spec :: Spec
spec = parallel $ do
    describe "Lexing test" Lex.spec
    describe "Parsing Test" Parse.spec
    describe "Infer Test" Infer.spec
