module Elara.Shunt.Error where

import Elara.AST.Name
import Elara.AST.New.Instances ()
import Elara.AST.New.Phases.Renamed qualified as NewR
import Elara.AST.New.Types qualified as New
import Elara.AST.Region (HasSourceRegion (sourceRegion), Located, SourceRegion, sourceRegionToDiagnosePosition)
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style
import Elara.Data.Unique
import Elara.Error
import Elara.Error.Codes qualified as Codes
import Elara.Shunt.Operator
import Error.Diagnose

-- | Helper to get the source region from a binary operator
binaryOpLoc :: New.BinaryOperator SourceRegion p -> SourceRegion
binaryOpLoc (New.SymOp loc _) = loc
binaryOpLoc (New.InfixedOp loc _) = loc

data ShuntError
    = SamePrecedenceError !(New.BinaryOperator SourceRegion NewR.Renamed, OpInfo) !(New.BinaryOperator SourceRegion NewR.Renamed, OpInfo)
    | UnknownOperator !Name !ModuleName
    | LocalOperatorInfoNotSupported !(Located (Unique Name))
    deriving (Show)

instance Exception ShuntError
instance ReportableError ShuntError where
    report (SamePrecedenceError (op1, a1) (op2, a2)) = do
        let op1Src = sourceRegionToDiagnosePosition $ binaryOpLoc op1
        let op2Src = sourceRegionToDiagnosePosition $ binaryOpLoc op2
        writeReport $
            Err
                (Just Codes.samePrecedence)
                ("Cannot mix operators with same precedence " <> prettyOp op1 <> " and " <> prettyOp op2 <> " when both operators have different associativity.")
                [(op1Src, This (pretty a1)), (op2Src, This (pretty a2))]
                [ Hint "Add parentheses to resolve the ambiguity"
                , Hint "Change the precedence of one of the operators"
                , Hint "Change the associativity of one of the operators"
                ]
    report (UnknownOperator name moduleName) = do
        writeReport $
            Err
                (Just Codes.unknownOperator)
                ( "Unknown operator "
                    <> Style.bold (Style.operator (pretty name))
                    <> " in module "
                    <> Style.moduleName (pretty moduleName)
                )
                []
                [Hint "Make sure the operator is defined in the module or imported from another module"]
    report (LocalOperatorInfoNotSupported name) = do
        let nameSrc = sourceRegionToDiagnosePosition $ name ^. sourceRegion
        writeReport $
            Err
                (Just Codes.localOperatorInfoNotSupported)
                ( "Cannot get operator info for local operator "
                    <> Style.bold (Style.operator (pretty (fmap (^. uniqueVal) name)))
                )
                [(nameSrc, This "local operator defined here")]
                [ Hint "Operator info can only be retrieved for global operators"
                , Note "This is a compiler limitation and may be lifted in future versions"
                ]

data ShuntWarning
    = UnknownPrecedence OpTable (Located Name)
    deriving (Show, Eq, Ord)

instance ReportableError ShuntWarning where
    report (UnknownPrecedence opTable operatorName) = do
        let opSrc = sourceRegionToDiagnosePosition $ operatorName ^. sourceRegion
        writeReport $
            Warn
                (Just Codes.unknownPrecedence)
                ( vsep
                    [ "Unknown precedence/associativity for operator" <+> pretty operatorName
                        <> ". The system will assume it has the highest precedence (9) and left associativity, but you should specify it manually. "
                    , "Known Operators:" <+> prettyOpTable opTable
                    ]
                )
                [(opSrc, This "operator declared here")]
                [Hint "Define the precedence and associativity of the operator explicitly. There is currently no way of doing this lol"]
