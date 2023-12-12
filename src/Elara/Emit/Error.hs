module Elara.Emit.Error where

import Elara.Core qualified as Core
import Elara.Core.Pretty ()
import Elara.Data.Pretty
import Elara.Data.Unique
import Elara.Error (ReportableError (report), writeReport)
import Elara.Error.Codes qualified as Codes
import Error.Diagnose (Note (Note), Report (..))

data EmitError where
    InvokeStaticLocal :: HasCallStack => Unique Text -> Core.Type -> EmitError

instance ReportableError EmitError where
    report (InvokeStaticLocal name t) = do
        writeReport $
            Err
                (Just Codes.invokeStaticLocal)
                ("Attempt to InvokeStatic a local definition:" <+> pretty name <+> ":" <+> pretty t)
                []
                [ Note "This is likely a compiler bug!"
                ]
