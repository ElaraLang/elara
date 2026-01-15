module Elara.Shunt.Error where

import Elara.AST.Generic.Types
import Elara.AST.Name
import Elara.AST.Region (HasSourceRegion (sourceRegion), Located, sourceRegionToDiagnosePosition)
import Elara.AST.Renamed (RenamedBinaryOperator, RenamedDeclarationBody)
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style
import Elara.Data.Unique
import Elara.Error
import Elara.Error.Codes qualified as Codes
import Elara.Shunt.Operator
import Error.Diagnose

data ShuntError
    = SamePrecedenceError !(RenamedBinaryOperator, OpInfo) !(RenamedBinaryOperator, OpInfo)
    | UnknownOperator !Name !ModuleName
    | LocalOperatorInfoNotSupported !(Located (Unique Name))
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
    = UnknownPrecedence OpTable RenamedDeclarationBody
    deriving (Show, Eq, Ord)

instance ReportableError ShuntWarning where
    report (UnknownPrecedence opTable lOperator) = do
        let opSrc = sourceRegionToDiagnosePosition $ lOperator ^. declarationBodyName % sourceRegion

        writeReport $
            Warn
                (Just Codes.unknownPrecedence)
                ( vsep
                    [ "Unknown precedence/associativity for operator" <+> pretty (lOperator ^. declarationBodyName)
                        <> ". The system will assume it has the highest precedence (9) and left associativity, but you should specify it manually. "
                    , "Known Operators:" <+> prettyOpTable opTable
                    ]
                )
                [(opSrc, This "operator declared here")]
                [Hint "Define the precedence and associativity of the operator explicitly. There is currently no way of doing this lol"]
