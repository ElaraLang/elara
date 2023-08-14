{- | During emitting, local variables are turned into normal JVM local variables where applicable.
 This requires a different binder to what 'Elara.Core.Expr' normally uses because we need to know the "index" of the local variable.
 This module handles that.
-}
module Elara.Emit.Var where

import Control.Lens (transform, transformOf, transformOn)
import Data.Data (Data, typeOf)
import Elara.Core (CoreExpr, Expr (..), Var, Bind (..))
import Elara.Core qualified as Core
import Elara.Core.Pretty (prettyVar)
import Elara.Data.Pretty
import Elara.AST.Pretty
import Print (debugColored, debugPretty, showPretty, showColored)

data JVMBinder
    = JVMLocal Int
    | Normal Var
    deriving (Eq, Show, Data)

instance Pretty JVMBinder where
    pretty = \case
        JVMLocal i -> "local_" <> pretty i
        Normal v -> prettyVar True True v

type JVMExpr = Expr JVMBinder

instance Pretty JVMExpr where
    pretty = \case
        Var v -> pretty v
        Lit l -> pretty l
        App e1 e2 -> parens (prettyFunctionCallExpr e1 e2)
        Lam b e -> prettyLambdaExpr [pretty b] e
        Let (Recursive binds) e ->
            "Rec"
                <+> prettyBlockExpr
                    ( fmap
                        ( \(bindName, bindVal) -> prettyLetInExpr (pretty bindName) none bindVal (Just e)
                        )
                        binds
                    )
        Let (NonRecursive (b, e)) e' -> prettyLetInExpr (pretty b) none e (Just e')
        Match e b alts ->
            let prettyAlts = fmap (\(con, _, b') -> prettyMatchBranch (con, b')) alts
             in "match"
                    <+> pretty e
                    <+> "as"
                    <+> pretty (pretty <$> b)
                    <+> "with"
                    <+> hardline
                        <> nest indentDepth (align (vsep prettyAlts))
        Type t -> "@" <> pretty t

toJVMExpr :: CoreExpr -> JVMExpr
toJVMExpr = fmap Normal

replaceVar :: JVMBinder -> JVMBinder -> JVMExpr -> JVMExpr
replaceVar old new = transform $ \case
    Core.Var old' | old == old' -> Core.Var new
    x -> error (showPretty (x, old))

{- | We end up with redundant top-level lambdas a lot, that can be converted into normal methods instead.
 For example, 'let add1 = \x -> x + 1' can be turned into `public static int add1(int x) { return x + 1; }`
 removing the need for allocating redundant closures.

 This function handles the transform, and renaming of Elara variables to @JVMBinder@s where applicable.
-}
transformTopLevelLambdas :: Monad m => CoreExpr -> m JVMExpr
transformTopLevelLambdas (Core.Lam p1 x) = do
    let e = toJVMExpr x
    pure $ replaceVar (Normal p1) (JVMLocal 0) e
-- TODO more lambdas
transformTopLevelLambdas x = pure $ toJVMExpr x