{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import CoreToCore qualified
import Golden qualified
import Infer qualified
import Lex qualified
import LiftClosures qualified
import Parse qualified
import Shunt qualified
import Test.Syd (Spec, describe, sydTest)
import ToANF qualified

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
    describe "Core-to-Core Test" CoreToCore.spec
    describe "ToANF Test" ToANF.spec

    describe "Golden Test" Golden.spec
