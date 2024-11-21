module Boilerplate where

import Elara.AST.Generic
import Elara.AST.Module
import Elara.AST.Name
import Elara.AST.Select
import Elara.AST.VarRef
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
import Polysemy (Sem, subsume_)
import Polysemy.Reader
import Region (qualifiedTest, testLocated)
import Hedgehog

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


-- pipelineResShouldSucceed :: (MonadIO m, Show a) => PipelineRes a -> m a
-- pipelineResShouldSucceed m = do
--     res <- liftIO m
--     case res of 
--         (_, Just x) -> pure x
--         (diag, Nothing) -> do
--             annotateShow diag
--             failure
    

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
