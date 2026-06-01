module Elara.Shunt.Error where

import Error.Diagnose hiding (Hint, Note)

import Elara.AST.Instances ()
import Elara.AST.Name
import Elara.AST.Region (HasSourceRegion (sourceRegion), Located, SourceRegion, sourceRegionToDiagnosePosition)
import Elara.Data.Pretty
import Elara.Data.Unique
import Elara.Error
import Elara.Error.Diagnose (toDiagnoseReports)
import Elara.Shunt.Operator

import Elara.AST.Phases.Renamed qualified as NewR
import Elara.AST.Types qualified as New
import Elara.Data.Pretty.Styles qualified as Style
import Elara.Error.Codes qualified as Codes

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

instance ElaraDiagnostic ShuntError where
    diagnosticMessage (SamePrecedenceError (op1, _) (op2, _)) =
        "Cannot mix operators with same precedence " <> prettyOp op1 <> " and " <> prettyOp op2 <> " when both operators have different associativity."
    diagnosticMessage (UnknownOperator name moduleName) =
        "Unknown operator "
            <> Style.bold (Style.operator (pretty name))
            <> " in module "
            <> Style.moduleName (pretty moduleName)
    diagnosticMessage (LocalOperatorInfoNotSupported name) =
        "Cannot get operator info for local operator "
            <> Style.bold (Style.operator (pretty (fmap (^. uniqueVal) name)))

    diagnosticCode (SamePrecedenceError _ _) = Just Codes.samePrecedence
    diagnosticCode (UnknownOperator _ _) = Just Codes.unknownOperator
    diagnosticCode (LocalOperatorInfoNotSupported _) = Just Codes.localOperatorInfoNotSupported

    diagnosticMarkers (SamePrecedenceError (op1, a1) (op2, a2)) =
        [ ElaraMarker (binaryOpLoc op1) PrimaryMarker (pretty a1)
        , ElaraMarker (binaryOpLoc op2) PrimaryMarker (pretty a2)
        ]
    diagnosticMarkers (UnknownOperator _ _) = []
    diagnosticMarkers (LocalOperatorInfoNotSupported name) = [ElaraMarker (name ^. sourceRegion) PrimaryMarker "local operator defined here"]

    diagnosticNotes (SamePrecedenceError _ _) =
        [ Hint "Add parentheses to resolve the ambiguity"
        , Hint "Change the precedence of one of the operators"
        , Hint "Change the associativity of one of the operators"
        ]
    diagnosticNotes (UnknownOperator _ _) = [Hint "Make sure the operator is defined in the module or imported from another module"]
    diagnosticNotes (LocalOperatorInfoNotSupported _) =
        [ Hint "Operator info can only be retrieved for global operators"
        , Note "This is a compiler limitation and may be lifted in future versions"
        ]

instance Pretty ShuntError where
    pretty = diagnosticMessage

instance Pretty ShuntWarning where
    pretty = diagnosticMessage

newtype ShuntWarning
    = UnknownPrecedence (Located (Qualified Name))
    deriving (Eq, Ord, Show)

instance ElaraDiagnostic ShuntWarning where
    diagnosticSeverity = const WarningSeverity
    diagnosticMessage (UnknownPrecedence operatorName) =
        "Unknown precedence/associativity for operator" <+> pretty operatorName
    diagnosticCode (UnknownPrecedence _) = Just Codes.unknownPrecedence
    diagnosticMarkers (UnknownPrecedence operatorName) = [ElaraMarker (operatorName ^. sourceRegion) PrimaryMarker "operator declared here"]
    diagnosticNotes (UnknownPrecedence operatorName) =
        [ Note $
            "Unknown precedence/associativity for operator" <+> pretty operatorName
                <> ". The system will assume it has the highest precedence (9) and left associativity, but you should specify it manually. "
        , Hint "Define the precedence and associativity of the operator explicitly, using #Fixity and #Associativity annotations"
        ]
