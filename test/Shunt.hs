module Shunt where

import Boilerplate
import Common (diagShouldSucceed)
import Elara.AST.Generic
import Elara.AST.Generic.Instances ()
import Elara.AST.Generic.Pattern (functionCall, int, var)
import Elara.AST.Select (UnlocatedAST (UnlocatedShunted))
import Elara.AST.StripLocation
import Elara.AST.Unlocated ()
import Hedgehog hiding (Var)
import HedgehogSyd ()
import Orphans ()
import Test.Syd (Spec, describe, it)
import Test.Syd.Hedgehog ()

shouldShuntTo :: (MonadTest m, MonadIO m, HasCallStack) => Text -> Expr UnlocatedShunted -> m ()
code `shouldShuntTo` x = withFrozenCallStack $ do
    y <- liftIO (loadShuntedExpr code) >>= diagShouldSucceed
    let z = stripLocation y
    z === x

spec :: Spec
spec = describe "Shunts operators correctly" $ do
    it "Shunts simple operators into prefix calls" $ property $ do
        let res =
                functionCall
                    ( functionCall
                        (var (stripLocation $ mkFakeVar "+"))
                        (int 1)
                    )
                    (int 2)
        "1 + 2" `shouldShuntTo` res
        "1 + (2)" `shouldShuntTo` res
        "(1) + 2" `shouldShuntTo` res
        "(1) + (2)" `shouldShuntTo` res
        "(1 + 2)" `shouldShuntTo` res
    it "Shunts repeated operators into prefix calls" $ property $ do
        let res =
                -- (+) (((+) 1) 2)) 3
                functionCall
                    ( functionCall
                        (var (stripLocation $ mkFakeVar "+"))
                        ( functionCall
                            ( functionCall
                                (var (stripLocation $ mkFakeVar "+"))
                                (int 1)
                            )
                            (int 2)
                        )
                    )
                    (int 3)
        "1 + 2 + 3" `shouldShuntTo` res -- becomes 1 + (2 + 3) which becomes (+) (((+) 1) 2)) 3
        "1 + (2) + 3" `shouldShuntTo` res
        "(1) + 2 + 3" `shouldShuntTo` res
        "(1) + (2) + (3)" `shouldShuntTo` res
        "(1 + 2 + 3)" `shouldShuntTo` res
        "((1 + 2) + 3))" `shouldShuntTo` res

    it "Correctly re-shunts operators with different precedences" $ property $ do
        let res =
                functionCall
                    ( functionCall
                        (var (stripLocation $ mkFakeVar "+"))
                        (int 1)
                    )
                    ( functionCall
                        ( functionCall
                            (var (stripLocation $ mkFakeVar "*"))
                            (int 2)
                        )
                        (int 3)
                    )
        "1 + 2 * 3" `shouldShuntTo` res -- becomes 1 + (2 * 3) which becomes (*) 1 ((+) 2 3)
        "1 + (2 * 3)" `shouldShuntTo` res -- because * has higher precedence, this is the same
        "1 + (2) * (3)" `shouldShuntTo` res -- because * has higher precedence, this is the same
        "(1 + (2 * 3))" `shouldShuntTo` res -- because * has higher precedence, this is the same
    it "Correctly re-shunts operators with different precedences when overrided by parentheses" $ property $ do
        let res =
                functionCall
                    ( functionCall
                        (var (stripLocation $ mkFakeVar "*"))
                        ( functionCall
                            ( functionCall
                                (var (stripLocation $ mkFakeVar "+"))
                                (int 1)
                            )
                            (int 2)
                        )
                    )
                    (int 3)
        "(1 + 2) * 3" `shouldShuntTo` res -- becomes (1 * 2) + 3 which becomes (+) ((*) 1 2) 3
        "(1 + 2) * (3)" `shouldShuntTo` res
        "((1 + 2)) * (3)" `shouldShuntTo` res
    it "Shunts the pipe operator properly" $ property $ do
        {-
            Given infixr 1 |>
            @1 |> 2 |> 3@ should become @1 |> (2 |> 3)@ which is @(|>) 1 ((|>) 2 3)@
            @1 |> (2 |> 3)@ should become @(|>) 1 ((|>) 2 3)@
            @(1 |> 2) |> 3@ should become @(|>) ((|>) 1 2) 3@
        -}
        let res =
                functionCall
                    ( functionCall
                        (var (stripLocation $ mkFakeVar "|>"))
                        (int 1)
                    )
                    ( functionCall
                        ( functionCall
                            (var (stripLocation $ mkFakeVar "|>"))
                            (functionCall (int 2) (int 3))
                        )
                        (functionCall (int 3) (int 4))
                    )

        "1 |> 2 3 |> (3 4)" `shouldShuntTo` res
        "1 |> (2 3) |> 3 4" `shouldShuntTo` res
        "1 |> (2 3 |> (3 4))" `shouldShuntTo` res
        "(1 |> 2) |> 3"
            `shouldShuntTo` functionCall
                ( functionCall
                    (var (stripLocation $ mkFakeVar "|>"))
                    ( functionCall
                        ( functionCall
                            (var (stripLocation $ mkFakeVar "|>"))
                            (int 1)
                        )
                        (int 2)
                    )
                )
                (int 3)
