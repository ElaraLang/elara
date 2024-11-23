{-# LANGUAGE TemplateHaskell #-}

module Boilerplate where

import Common (diagShouldSucceed)
import Elara.AST.Generic
import Elara.AST.Module
import Elara.AST.Name hiding (Name)
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
import Language.Haskell.TH

import Hedgehog.Internal.Property (failDiff, failWith)
import Hedgehog.Internal.Show (mkValue, renderValue)
import Language.Haskell.TH (pprint)
import Language.Haskell.TH.Lib (stringE)
import Language.Haskell.TH.Syntax (Exp, Lift, Name (..), NameFlavour (..), Pat, Q)
import Polysemy (Sem, subsume_)
import Polysemy.Reader
import Print
import Print (showColored)
import Region (qualifiedTest, testLocated)
import Test.Syd (Expectation, expectationFailure)
import Text.Show

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

ensureExpressionMatches :: HasCallStack => Q Pat -> Q Exp
ensureExpressionMatches qpat = do
    pat <- qpat
    let msg = Shown $ pprint (stripQualifiers pat)

    [e|\case $(pure pat) -> pure (); o -> withFrozenCallStack $ failDiff msg o|]

newtype Shown = Shown String deriving (Lift)

instance Show Shown where
    show (Shown s) = s

stripQualifiers :: Pat -> Pat
stripQualifiers = transformOf gplate $ \case
    ConP name t ps -> ConP (stripQualifiersName name) t ps
    x -> x
  where
    stripQualifiersName :: Name -> Name
    stripQualifiersName (Name x (NameQ _)) = Name x NameS
    stripQualifiersName (Name x (NameG{})) = Name x NameS
    stripQualifiersName n = n
