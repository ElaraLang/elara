{- | During emitting, local variables are turned into normal JVM local variables where applicable.
This requires a different binder to what 'Elara.Core.Expr' normally uses because we need to know the "index" of the local variable.
This module handles that.
-}
module Elara.Emit.Var where

import Control.Lens (transform)
import Data.Data (Data)
import Elara.AST.VarRef (UnlocatedVarRef)
import Elara.Core (CoreExpr, Expr (..), Var)
import Elara.Core qualified as Core
import Elara.Core.Pretty (PrettyVar (prettyVarArg), prettyVar)
import Elara.Data.Pretty

data JVMBinder
    = JVMLocal !Word8
    | Normal !Var
    deriving (Eq, Show, Data, Generic)

instance Hashable JVMBinder

instance Pretty JVMBinder where
    pretty = prettyVar True True
instance PrettyVar JVMBinder where
    prettyVar t p (Normal v) = prettyVar t p v
    prettyVar _ _ (JVMLocal i) = "local_" <> pretty i

    prettyVarArg = prettyVar True True

type JVMExpr = Expr JVMBinder

toJVMExpr :: CoreExpr -> JVMExpr
toJVMExpr = fmap Normal

replaceVar :: JVMBinder -> JVMBinder -> JVMExpr -> JVMExpr
replaceVar old new = transform $ \case
    Core.Var (Normal (Core.Id old' _)) | (Normal (Core.Id old'' _)) <- old, old' == old'' -> Core.Var new -- compare ignoring types
    Core.Var old' | old == old' -> Core.Var new
    x -> x

replaceVar' :: UnlocatedVarRef Text -> JVMBinder -> JVMExpr -> JVMExpr
replaceVar' old new = transform $ \case
    Core.Var (Normal (Core.Id old' _)) | old == old' -> Core.Var new
    x -> x

{- | We end up with redundant top-level lambdas a lot, that can be converted into normal methods instead.
For example, 'let add1 = \x -> x + 1' can be turned into `public static int add1(int x) { return x + 1; }`
removing the need for allocating redundant closures.

This function handles the transform, and renaming of Elara variables to @JVMBinder@s where applicable.
-}
transformTopLevelLambdas :: CoreExpr -> JVMExpr
transformTopLevelLambdas = transformTopLevelJVMLambdas . toJVMExpr

transformTopLevelJVMLambdas :: JVMExpr -> JVMExpr
transformTopLevelJVMLambdas = go 0
  where
    go :: Word8 -> JVMExpr -> JVMExpr
    go c (Lam (Normal v@(Core.Id _ _)) body) = replaceVar (Normal v) (JVMLocal c) (go (c + 1) body)
    go _ x = x

-- {- | When we have a function let x = y, where y : A -> B,
-- We have to convert this into public static B _x(A a) { return y(a); }
-- This function adds the application of the function to the argument, i.e. turning it into let x = y local_0,
-- meaning it will be compiled correctly.

-- > addTopLevelLambdas (identity, a -> a) = identity local_0
-- > addTopLevelLambdas (local_0, a -> a) = local_0
-- -}
-- addTopLevelLambdas :: (CoreExpr, Core.Type) -> JVMExpr
-- addTopLevelLambdas (funcBody, funcType) = go 0 (funcBody, funcType)
--   where
--     go :: Word8 -> (CoreExpr, Core.Type) -> JVMExpr
--     go c (a, b) | trace ("addTopLevelLambdas: " <> show (pretty a, pretty b)) False = undefined
--     go c (App x y, stripForAll -> Core.FuncTy i o) =
--         App (go c (x, o)) (toJVMExpr y)
--     go c (body, stripForAll -> Core.FuncTy _ o) =
--         App (go (c + 1) (body, o)) (Var $ JVMLocal c)
--     go _ (x, y) = toJVMExpr x

{-
let id = identity

let add1 = add 1

==>

let id : a -> a = identity

let add1 : Int -> Int = add 1

==>

public static Object _id(Object x) {
    return _identity(x)
}

public static Int _add1(Int x) {
    return _add(1, x)
}
-}
