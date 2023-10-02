module Shunt where

import Common (diagShouldSucceed)
import Elara.AST.Generic
import Elara.AST.Generic.Instances ()
import Elara.AST.Generic.Pattern (functionCall, int, var)
import Elara.AST.Name (OpName (..), Qualified (..), VarName (OperatorVarName))
import Elara.AST.Region (generatedLocated)
import Elara.AST.Select (LocatedAST (..), UnlocatedAST (UnlocatedShunted))
import Elara.AST.StripLocation
import Elara.AST.Unlocated ()
import Elara.AST.VarRef (VarRef, VarRef' (Global), withName)
import Elara.Data.TopologicalGraph
import Elara.Desugar (desugarExpr, runDesugar, runDesugarPipeline)
import Elara.Lexer.Pipeline (runLexPipeline)
import Elara.Lexer.Reader (readTokensWith)
import Elara.Parse (parsePipeline, runParsePipeline)
import Elara.Parse.Expression (exprParser)
import Elara.Pipeline (PipelineRes, finalisePipeline)
import Elara.Prim.Rename (primitiveRenameState)
import Elara.Rename (RenameState (..), renameExpr, runRenamePipeline)
import Elara.Shunt (Associativity (..), OpInfo (OpInfo), OpTable, fixExpr, mkPrecedence, runShuntPipeline)
import Hedgehog hiding (Var)
import Orphans ()
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

loadExpr :: Text -> PipelineRes (Expr 'Shunted)
loadExpr source = finalisePipeline . runShuntPipeline fakeOperatorTable . runRenamePipeline (createGraph []) operatorRenameState . runParsePipeline . runLexPipeline $ do
    let fp = "<tests>"
    tokens <- readTokensWith fp (toString source)
    parsed <- parsePipeline exprParser fp tokens
    desugared <- runDesugarPipeline $ runDesugar $ desugarExpr parsed

    renamed <- renameExpr desugared
    fixExpr renamed

mkFakeVar :: OpName -> VarRef VarName
mkFakeVar name = Global (generatedLocated Nothing (Qualified (OperatorVarName name) "ShuntTests"))

mkFakeOp :: OpName -> VarRef OpName
mkFakeOp name = Global (generatedLocated Nothing (Qualified name "ShuntTests"))

operatorRenameState :: RenameState
operatorRenameState =
    let mkFakeVarP name = (OperatorVarName name, mkFakeVar name)
     in primitiveRenameState
            <> RenameState
                { varNames =
                    fromList
                        [ mkFakeVarP "+"
                        , mkFakeVarP "-"
                        , mkFakeVarP "*"
                        , mkFakeVarP "/"
                        ]
                , typeNames = fromList []
                , typeVars = fromList []
                }

fakeOperatorTable :: OpTable
fakeOperatorTable =
    let mkFakeVarP name info = (withName $ mkFakeOp name, info)
     in fromList
            [ mkFakeVarP "+" (OpInfo (mkPrecedence 6) LeftAssociative)
            , mkFakeVarP "-" (OpInfo (mkPrecedence 6) LeftAssociative)
            , mkFakeVarP "*" (OpInfo (mkPrecedence 7) LeftAssociative)
            , mkFakeVarP "/" (OpInfo (mkPrecedence 7) LeftAssociative)
            ]

shouldShuntTo :: (MonadTest m, MonadIO m, HasCallStack) => Text -> Expr 'UnlocatedShunted -> m ()
code `shouldShuntTo` x = withFrozenCallStack $ do
    y <- liftIO (loadExpr code >>= diagShouldSucceed)
    let z :: Expr 'UnlocatedShunted = stripLocation y
    z === x

spec :: Spec
spec = describe "Shunts operators correctly" $ do
    it "Shunts simple operators into prefix calls" $
        hedgehog $ do
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

    it "Correctly re-shunts operators with different precedences" $ hedgehog $ do
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
    it "Correctly re-shunts operators with different precedences when overrided by parentheses" $ hedgehog $ do
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
