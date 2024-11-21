module Boilerplate where

import Common (diagShouldSucceed)
import Elara.AST.Generic
import Elara.AST.Module
import Elara.AST.Name
import Elara.AST.Select
import Elara.AST.VarRef
import Elara.Data.Pretty (prettyToText)
import Elara.Data.TopologicalGraph
import Elara.Desugar
import Elara.Lexer.Pipeline
import Elara.Lexer.Reader
import Elara.Parse
import Elara.Parse.Expression
import Elara.Pipeline
import Elara.Prim.Rename
import Elara.Rename
import Elara.Shunt
import Error.Diagnose (hasReports)
import Error.Diagnose.Diagnostic
import Hedgehog
import Polysemy (Sem, subsume_)
import Polysemy.Reader
import Region (qualifiedTest, testLocated)
import Test.Syd (Expectation, expectationFailure)

loadRenamedExpr :: Text -> PipelineRes (Expr 'Renamed)
loadRenamedExpr = finalisePipeline . loadRenamedExpr'

loadRenamedExpr' :: _ => Text -> Sem _ (Expr 'Renamed)
loadRenamedExpr' source = runRenamePipeline (createGraph []) operatorRenameState . runParsePipeline . runLexPipeline $ do
    let fp = "<tests>"
    tokens <- readTokensWith fp (toString source)
    parsed <- parsePipeline exprParser fp (toString source, tokens)
    desugared <- runDesugarPipeline $ runDesugar $ desugarExpr parsed

    runReader Nothing $ runReader (Nothing @(Module 'Desugared)) $ renameExpr desugared

loadShuntedExpr :: Text -> PipelineRes (Expr 'Shunted)
loadShuntedExpr source = finalisePipeline . runShuntPipeline $ do
    renamed <- loadRenamedExpr' source
    runReader fakeOperatorTable $ fixExpr renamed

pipelineResShouldSucceed :: (Show a, _) => PipelineRes a -> IO a
pipelineResShouldSucceed m = withFrozenCallStack $ do
    (d, x) <- m
    when (hasReports d) $
        expectationFailure $
            toString $
                prettyToText $
                    prettyDiagnostic' WithUnicode (TabSize 4) d
    case x of
        Just ok -> pure ok
        Nothing -> expectationFailure $ toString $ prettyToText $ prettyDiagnostic' WithUnicode (TabSize 4) d

operatorRenameState :: RenameState
operatorRenameState =
    let mkFakeVarP name = (OperatorVarName name, one $ mkFakeVar name)
     in primitiveRenameState
            <> RenameState
                { varNames =
                    fromList
                        [ mkFakeVarP "+"
                        , mkFakeVarP "-"
                        , mkFakeVarP "*"
                        , mkFakeVarP "/"
                        , mkFakeVarP "|>"
                        ]
                , typeNames = fromList []
                , typeVars = fromList []
                }

fakeOperatorTable :: OpTable
fakeOperatorTable =
    let mkFakeVarP name info = (ignoreLocation $ withName $ mkFakeOp name, info)
     in fromList
            [ mkFakeVarP "+" (OpInfo (mkPrecedence 6) LeftAssociative)
            , mkFakeVarP "-" (OpInfo (mkPrecedence 6) LeftAssociative)
            , mkFakeVarP "*" (OpInfo (mkPrecedence 7) LeftAssociative)
            , mkFakeVarP "/" (OpInfo (mkPrecedence 7) LeftAssociative)
            , mkFakeVarP "|>" (OpInfo (mkPrecedence 1) RightAssociative)
            ]

mkFakeVar :: OpName -> VarRef VarName
mkFakeVar name = Global (testLocated (qualifiedTest (OperatorVarName name)))

mkFakeOp :: OpName -> VarRef OpName
mkFakeOp name = Global (testLocated (qualifiedTest name))
