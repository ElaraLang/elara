import Test.DocTest
import Test.DocTest.Helpers

main :: IO ()
main = do
  args <- getArgs
  lib <- extractCabalLibrary "elara.cabal"

  let removeLex = lib {libModules = libModules lib `rmList` ["Elara.Lexer.Lexer"]} -- this module breaks stuff
  mainFromLibrary removeLex args
