import ConstExpr qualified
import DataUnwrap qualified
import Golden qualified
import Infer qualified
import Lex qualified
import LiftClosures qualified
import Parse qualified
import Shunt qualified
import TopologicalGraph qualified
import Utils qualified
import Width qualified
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
    describe "Closure Lifting Test" LiftClosures.spec

    describe "Golden Test" Golden.spec

    -- New utility tests
    describe "Utils Test" Utils.spec
    describe "Width Test" Width.spec
    describe "Data.Unwrap Test" DataUnwrap.spec
    describe "ConstExpr Test" ConstExpr.spec
    describe "TopologicalGraph Test" TopologicalGraph.spec
