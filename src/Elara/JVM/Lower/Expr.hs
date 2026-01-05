module Elara.JVM.Lower.Expr (lowerExpr) where

import Effectful
import Effectful.Error.Static (throwError)
import Elara.AST.Name (Qualified (..), unqualified)
import Elara.AST.VarRef
import Elara.Core qualified as Core
import Elara.Core.Analysis
import Elara.Core.Generic qualified as Core
import Elara.JVM.Error (JVMLoweringError (..))
import Elara.JVM.IR qualified as IR
import Elara.JVM.Lower.Function
import Elara.JVM.Lower.Match
import Elara.JVM.Lower.Monad
import Elara.JVM.Lower.Util
import Elara.Logging
import JVM.Data.Abstract.Descriptor qualified as JVM
import JVM.Data.Abstract.Type qualified as JVM
import JVM.Data.Convert (jloName)
import Print (showPretty)

lowerExpr :: InnerLower r => Core.CoreExpr -> Eff r IR.Expr
lowerExpr expr = case expr of
    -- Erase type applications
    Core.TyApp e _ -> lowerExpr e
    Core.Lit lit -> pure (lowerLiteral lit)
    Core.Var _ -> unwindCall expr []
    Core.App _ _ -> unwindCall expr []
    Core.Let (Core.NonRecursive (Core.Id (Local name) idType _, val)) body -> do
        valExpr <- lowerExpr val
        -- If the value is a closure/wrapper, use the interface type, otherwise use the variable type
        let ty = case valExpr of
                IR.MakeClosure{IR.closureInterface = iface} -> JVM.ObjectFieldType iface
                IR.MakeConstructorClosure{IR.ctorClosureInterface = iface} -> JVM.ObjectFieldType iface
                _ -> lowerType idType
        emitInst $ IR.Assign name ty valExpr
        lowerExpr body
    Core.Match scrutinee binder alts -> do
        ty <- traceFn guesstimateExprType expr
        lowerMatch lowerExpr ty scrutinee binder alts
    other -> throwError $ UnsupportedExpressionType other

{- | Recursively unwind an application chain to find the function being called and the arguments,
then lower the call appropriately.

For example, given the expression: @f x y z@, this function will extract @f@ and the arguments @[x, y, z]@,
and lower the call to @f@ with those arguments.
-}
unwindCall :: InnerLower r => Core.CoreExpr -> [IR.Expr] -> Eff r IR.Expr
unwindCall (Core.App f x) args = do
    x' <- lowerExpr x
    unwindCall f (x' : args)
unwindCall (Core.TyApp f _) args = unwindCall f args
unwindCall (Core.Var (Core.Id (Local name) type_ _)) [] = do
    -- no arguments, just a local variable
    -- so we just return it directly
    pure $ IR.LocalVar name (lowerType type_)
unwindCall (Core.Var (Core.Id source type_ dataCon)) args = do
    let argTys = map lowerType (Core.functionTypeArgs type_)
    let retTy = lowerType (Core.functionTypeResult type_)
    let desc = JVM.MethodDescriptor argTys (JVM.TypeReturn retTy)
    let fullArity = length argTys

    target <- case (source, dataCon) of
        (_, Just (Core.DataCon conName _ _)) -> do
            let constructorDesc = JVM.MethodDescriptor argTys JVM.VoidReturn
            pure $ Constructor (qualifiedTextToClass conName) constructorDesc
        (Global qn, _) ->
            pure $ StaticMethod (moduleNameToQualifiedClassName $ qualifier qn) (qn ^. unqualified) desc
        (Local name, _) -> do
            let localVar = IR.LocalVar name (JVM.ObjectFieldType jloName)
            pure $ InstanceMethod localVar desc

    let callable = CallableInfo target retTy fullArity
    lowerCallable callable args AsCall
unwindCall other _ = throwError $ AppOfNonFunction (showPretty other)
