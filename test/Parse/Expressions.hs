module Parse.Expressions where

import Arbitrary.AST (genExpr)
import Control.Lens (transform, transformOn, (^.), _1)
import Data.Generics.Wrapped
import Elara.AST.Generic
import Elara.AST.Name
import Elara.AST.Select
import Elara.AST.StripLocation
import Elara.Parse.Expression (exprParser)
import Hedgehog hiding (Var)
import Orphans ()
import Parse.Common
import Print (showPrettyUnannotated)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = parallel $ describe "Parses expressions correctly" $ do
  arbitraryExpr
  weirdEdgeCases

weirdEdgeCases :: Spec
weirdEdgeCases = describe "Parses some weird edge cases correctly" $ do
  it "Parses the funky lambda thing properly" $ hedgehog $ do
    "(\\x -> x + 2) 3"
      `shouldParseExpr` Expr
        ( FunctionCall
            ( Expr
                ( ( ( Lambda
                        [Pattern (VarPattern (LowerAlphaName "x"), Nothing)]
                        ( Expr
                            ( BinaryOperator
                                ( ( MkBinaryOperator (SymOp "+"),
                                    Expr (Var (MaybeQualified "x" Nothing), Nothing),
                                    Expr (Int 2, Nothing)
                                  )
                                ),
                              Nothing
                            )
                        )
                    )
                  ),
                  Nothing
                )
            )
            (Expr (Int 3, Nothing)),
          Nothing
        )

arbitraryExpr :: Spec
arbitraryExpr = it "Arbitrary expressions parse prettyPrinted" $ hedgehog $ do
  expr <- forAll genExpr
  let parsePretty s = fmap (removeInParens . stripLocation) <$> lexAndParse exprParser s
  trippingParse (removeInParens expr) (showPrettyUnannotated . removeInParens) parsePretty
  where
    -- The AST needs to have the 'InParens' element for operator shunting later, but its presence messes up the pretty printing & parsing equality
    -- This just removes any 'InParens' elements from the AST
    removeInParens :: Expr 'UnlocatedFrontend -> Expr 'UnlocatedFrontend
    removeInParens = transformOn (_Unwrapped . _1) go
      where
        go :: Expr' 'UnlocatedFrontend -> Expr' 'UnlocatedFrontend
        go (InParens e) = e ^. _Unwrapped . _1
        go e = e
