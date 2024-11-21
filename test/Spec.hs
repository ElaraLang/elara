import Infer qualified
import Lex qualified
import Parse qualified
import Shunt qualified
import Test.Syd (Spec, describe, sydTest)

main :: IO ()
main = do
    sydTest spec

spec :: Spec
spec = do
    describe "Lexing test" Lex.spec
    describe "Parsing Test" Parse.spec
    describe "Infer Test" Infer.spec
    describe "Shunt Test" Shunt.spec
