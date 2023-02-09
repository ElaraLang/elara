module Elara.AST.Annotated where

{-|
  This is the second main AST stage, which is very similar to the `Elara.AST.Frontend.Expr` AST, with a few key differences:

    * Everything is explicitly qualified with its module name (if applicable)
    * Lambdas only have 1 argument
-}
data Expr'
    = Int Integer