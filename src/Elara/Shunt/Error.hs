module Elara.Shunt.Error where

import Elara.AST.Generic.Types
import Elara.AST.Region (HasSourceRegion (sourceRegion), sourceRegionToDiagnosePosition)
import Elara.AST.Renamed (RenamedBinaryOperator)
import Elara.Data.Pretty
import Elara.Error
import Elara.Error.Codes qualified as Codes
import Elara.Shunt.Operator
import Error.Diagnose

data ShuntError
    = SamePrecedenceError !(RenamedBinaryOperator, OpInfo) !(RenamedBinaryOperator, OpInfo)
    deriving (Show)

instance Exception ShuntError
instance ReportableError ShuntError where
    report (SamePrecedenceError (op1@(MkBinaryOperator op1'), a1) (op2@(MkBinaryOperator op2'), a2)) = do
        let op1Src = sourceRegionToDiagnosePosition $ op1' ^. sourceRegion
        let op2Src = sourceRegionToDiagnosePosition $ op2' ^. sourceRegion
        writeReport $
            Err
                (Just Codes.samePrecedence)
                ("Cannot mix operators with same precedence " <> prettyOp op1 <> " and " <> prettyOp op2 <> " when both operators have different associativity.")
                [(op1Src, This (pretty a1)), (op2Src, This (pretty a2))]
                [Hint "Add parentheses to resolve the ambiguity", Hint "Change the precedence of one of the operators", Hint "Change the associativity of one of the operators"]

data ShuntWarning
    = UnknownPrecedence OpTable RenamedBinaryOperator
    deriving (Eq, Ord, Show)

instance ReportableError ShuntWarning where
    report (UnknownPrecedence opTable lop@(MkBinaryOperator lOperator)) = do
        let opSrc = sourceRegionToDiagnosePosition $ lOperator ^. sourceRegion
        writeReport $
            Warn
                (Just Codes.unknownPrecedence)
                ( vsep
                    [ "Unknown precedence/associativity for operator" <+> prettyOp lop
                        <> ". The system will assume it has the highest precedence (9) and left associativity, but you should specify it manually. "
                    , "Known Operators:" <+> prettyOpTable opTable
                    ]
                )
                [(opSrc, This "operator")]
                [Hint "Define the precedence and associativity of the operator explicitly. There is currently no way of doing this lol"]
