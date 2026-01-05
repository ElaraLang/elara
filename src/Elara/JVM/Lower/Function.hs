module Elara.JVM.Lower.Function (CallableInfo (..), CallableTarget (..), CallStrategy (..), CallMode (..), analyzeCallStrategy, lowerCallable) where

import Effectful
import Effectful.Error.Static
import Elara.Data.Pretty (Pretty)
import Elara.JVM.Error (JVMLoweringError (..))
import Elara.JVM.IR qualified as IR
import Elara.JVM.Lower.Monad
import Elara.JVM.Lower.Util
import JVM.Data.Abstract.Descriptor qualified as JVM
import JVM.Data.Abstract.Name qualified as JVM
import JVM.Data.Abstract.Type qualified as JVM

-- | Represents a callable entity (function, closure, constructor)
data CallableInfo = CallableInfo
    { callableTarget :: CallableTarget
    , callableReturnType :: JVM.FieldType
    , callableArity :: Int
    }

data CallableTarget
    = StaticMethod JVM.QualifiedClassName Text JVM.MethodDescriptor
    | InstanceMethod IR.Expr JVM.MethodDescriptor
    | Constructor JVM.QualifiedClassName JVM.MethodDescriptor
    deriving (Generic)
instance Pretty CallableTarget

-- | How to handle a call based on arity analysis
data CallStrategy
    = -- | Fully saturated, call directly
      DirectCall
    | -- | Under-saturated, create closure wrapper
      CreateClosure
    | -- | Over-saturated (usually an error)
      OverApplication
    deriving (Generic)

instance Pretty CallStrategy

-- | Context for lowering a callable: method declaration vs call site
data CallMode
    = -- | A function/method declaration (can eta-expand)
      AsDeclaration
    | -- | A call site, cannot be eta-expanded
      AsCall

-- | Determine the call strategy based on actual vs expected arguments
analyzeCallStrategy :: Int -> Int -> CallStrategy
analyzeCallStrategy actual expected = case compare actual expected of
    EQ -> DirectCall
    LT -> CreateClosure
    GT -> OverApplication

lowerCallable :: InnerLower r => CallableInfo -> [IR.Expr] -> CallMode -> Eff r IR.Expr
lowerCallable (CallableInfo target retTy arity) args mode = do
    let actualArity = length args
    let strategy = analyzeCallStrategy actualArity arity

    case (strategy, mode, target) of
        (DirectCall, AsCall, InstanceMethod expr _) | arity == 0 -> pure expr
        (DirectCall, _, _) -> lowerDirectCall target retTy args
        (CreateClosure, AsCall, _) -> lowerPartialApplication target retTy arity args
        (OverApplication, _, _) -> do
            throwError $
                OverApplicationOf
                    ( case target of
                        StaticMethod _ n desc -> (n, desc)
                        InstanceMethod _ desc -> ("run", desc)
                        Constructor _ desc -> ("constructor", desc)
                    )
                    actualArity
                    arity
        (CreateClosure, AsDeclaration, _) ->
            error "Internal error: declaration mode should handle eta expansion separately"

-- | Lower a fully saturated call
lowerDirectCall :: InnerLower r => CallableTarget -> JVM.FieldType -> [IR.Expr] -> Eff r IR.Expr
lowerDirectCall target retTy args = case target of
    StaticMethod cls name desc ->
        pure $ IR.Call (IR.InvokeStatic cls name desc) args
    Constructor cls (JVM.MethodDescriptor argTypes _) ->
        pure $ IR.New cls (zip args argTypes)
    InstanceMethod callee _ -> do
        let arity = length args
        let iface = funcInterfaceName arity
        let desc = erasedMethodDescriptor arity
        pure $ IR.Cast (IR.Call (IR.InvokeInterface callee iface "run" desc) args) retTy

-- | Extract argument types from a callable target
targetArgTypes :: CallableTarget -> [JVM.FieldType]
targetArgTypes = \case
    StaticMethod _ _ (JVM.MethodDescriptor as _) -> as
    Constructor _ (JVM.MethodDescriptor as _) -> as
    InstanceMethod _ (JVM.MethodDescriptor as _) -> as

-- | Lower a partial application into a closure
lowerPartialApplication :: InnerLower r => CallableTarget -> JVM.FieldType -> Int -> [IR.Expr] -> Eff r IR.Expr
lowerPartialApplication target _retTy fullArity capturedArgs = do
    let capturedCount = length capturedArgs
    let remainingArity = fullArity - capturedCount

    if remainingArity == 0
        then lowerDirectCall target _retTy capturedArgs
        else do
            let iface = funcInterfaceName remainingArity
            let capturedTypes = take capturedCount (targetArgTypes target)
            let captures = zip capturedArgs capturedTypes

            case target of
                StaticMethod cls name desc ->
                    pure $ IR.MakeClosure cls name desc iface captures
                Constructor cls desc ->
                    pure $ IR.MakeConstructorClosure cls desc iface captures
                InstanceMethod callee _ -> do
                    let callIface = funcInterfaceName fullArity
                    let callDesc = erasedMethodDescriptor capturedCount
                    let retTy' = JVM.ObjectFieldType (funcInterfaceName remainingArity)
                    pure $ IR.Cast (IR.Call (IR.InvokeInterface callee callIface "run" callDesc) capturedArgs) retTy'
