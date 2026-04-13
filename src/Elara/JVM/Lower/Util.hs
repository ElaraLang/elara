module Elara.JVM.Lower.Util where

import Effectful
import Effectful.Writer.Static.Local
import Elara.AST.Name
import Elara.Core qualified as Core
import Elara.Data.Unique
import Elara.Data.Unique.Effect
import Elara.JVM.IR qualified as IR
import Elara.JVM.Lower.Monad
import Elara.Prim (OpaquePrim (..))
import JVM.Data.Abstract.Descriptor qualified as JVM
import JVM.Data.Abstract.Name
import JVM.Data.Abstract.Type qualified as JVM
import JVM.Data.Convert (jloName)

qualifiedTextToClass :: Qualified Text -> QualifiedClassName
qualifiedTextToClass qn =
    let (mod, name) = (qualifier qn, qn ^. unqualified)
     in QualifiedClassName (moduleNameToPackage mod) (ClassName name)
  where
    moduleNameToPackage (ModuleName parts) = PackageName (toList parts)

{- | Lower all the arguments of a function type.
"Int -> String -> Person" ==> [Int, String]
-}
extractFieldTypes :: Core.Type -> [JVM.FieldType]
extractFieldTypes = fmap lowerType . Core.functionTypeArgs

lowerType :: Core.Type -> JVM.FieldType
lowerType t = case t of
    Core.TyVarTy _ ->
        JVM.ObjectFieldType "java/lang/Object" -- erase all type variables to Object
    Core.FuncTy _ _ ->
        JVM.ObjectFieldType "Elara/Func" -- todo: what about arity?
    Core.AppTy con _ ->
        lowerType con
    Core.ForAllTy _ inner ->
        lowerType inner -- erase forall quantifiers
    Core.ConTy (Core.TyCon name details) ->
        case details of
            Core.Prim p ->
                lowerPrimType p
            Core.TyADT _ ->
                JVM.ObjectFieldType (qualifiedTextToClass name)
            Core.TyAlias inner ->
                lowerType inner

-- | Map an opaque primitive directly to its JVM type
lowerPrimType :: OpaquePrim -> JVM.FieldType
lowerPrimType = \case
    PrimInt -> JVM.ObjectFieldType "java.lang.Integer"
    PrimString -> JVM.ObjectFieldType "Elara.String"
    PrimChar -> JVM.ObjectFieldType "java.lang.Character"
    PrimDouble -> JVM.ObjectFieldType "java.lang.Double"
    PrimFloat -> JVM.ObjectFieldType "java.lang.Float"
    PrimIO -> JVM.ObjectFieldType "Elara.IO"

-- | Generate field name for constructor field by index
fieldNameForIndex :: Int -> Text
fieldNameForIndex i = "f" <> show i

-- | Generates a function interface name for a given arity
funcInterfaceName :: Int -> QualifiedClassName
funcInterfaceName arity =
    let name = if arity == 1 then "Func" else "Func" <> show arity
     in QualifiedClassName (PackageName ["Elara"]) (ClassName name)

-- | Creates a (Object, Object...) -> Object descriptor for type-erased calls
erasedMethodDescriptor :: Int -> JVM.MethodDescriptor
erasedMethodDescriptor arity =
    let obj = JVM.ObjectFieldType jloName
     in JVM.MethodDescriptor (replicate arity obj) (JVM.TypeReturn obj)

moduleNameToQualifiedClassName :: ModuleName -> QualifiedClassName
moduleNameToQualifiedClassName (ModuleName name) =
    QualifiedClassName (PackageName $ init name) (ClassName $ last name)

freshVar :: Lower r => Eff r (Unique Text)
freshVar = makeUnique "v"

lowerLiteral :: Core.Literal -> IR.Expr
lowerLiteral = \case
    Core.Int i -> IR.LitInt i
    Core.String s -> IR.LitString s
    Core.Char c -> IR.LitChar c
    Core.Double d -> IR.LitDouble d
    Core.Unit -> IR.LitUnit

captureInstructions ::
    Eff (Writer ([IR.Instruction], [IR.Block]) : r) a ->
    Eff r (a, ([IR.Instruction], [IR.Block]))
captureInstructions = runWriter
