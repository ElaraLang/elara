module Elara.JVM.Lower.Util where

import Effectful
import Effectful.Writer.Static.Local
import H2JVM
import H2JVM.Internal.Convert
import H2JVM.Name

import Elara.AST.Name
import Elara.Data.Unique
import Elara.Data.Unique.Effect
import Elara.JVM.Lower.Monad
import Elara.Prim (OpaquePrim (..))

import Elara.Core qualified as Core
import Elara.JVM.IR qualified as IR

qualifiedTextToClass :: Qualified Text -> QualifiedClassName
qualifiedTextToClass qn =
    let (mod, name) = (qualifier qn, qn ^. unqualified)
     in QualifiedClassName (moduleNameToPackage mod) (parseClassName name)
  where
    moduleNameToPackage (ModuleName parts) = PackageName (toList parts)

{- | Lower all the arguments of a function type.
"Int -> String -> Person" ==> [Int, String]
-}
extractFieldTypes :: Core.Type -> [FieldType]
extractFieldTypes = fmap lowerType . Core.functionTypeArgs

lowerType :: Core.Type -> FieldType
lowerType t = case t of
    Core.TyVarTy _ ->
        ObjectFieldType "java/lang/Object" -- erase all type variables to Object
    Core.FuncTy _ _ ->
        ObjectFieldType "Elara/Func" -- todo: what about arity?
    Core.AppTy con _ ->
        lowerType con
    Core.ForAllTy _ inner ->
        lowerType inner -- erase forall quantifiers
    Core.ConTy (Core.TyCon name details) ->
        case details of
            Core.Prim p ->
                lowerPrimType p
            Core.TyADT _ ->
                ObjectFieldType (qualifiedTextToClass name)
            Core.TyAlias inner ->
                lowerType inner

-- | Map an opaque primitive directly to its JVM type
lowerPrimType :: OpaquePrim -> FieldType
lowerPrimType = \case
    PrimInt -> ObjectFieldType "java.lang.Integer"
    PrimString -> ObjectFieldType "Elara.String"
    PrimChar -> ObjectFieldType "java.lang.Character"
    PrimDouble -> ObjectFieldType "java.lang.Double"
    PrimFloat -> ObjectFieldType "java.lang.Float"
    PrimIO -> ObjectFieldType "Elara.IO"

-- | Generate field name for constructor field by index
fieldNameForIndex :: Int -> Text
fieldNameForIndex i = "f" <> show i

-- | Generates a function interface name for a given arity
funcInterfaceName :: Int -> QualifiedClassName
funcInterfaceName arity =
    let name = if arity == 1 then "Func" else "Func" <> show arity
     in QualifiedClassName (PackageName ["Elara"]) (parseClassName name)

-- | Creates a (Object, Object...) -> Object descriptor for type-erased calls
erasedMethodDescriptor :: Int -> MethodDescriptor
erasedMethodDescriptor arity =
    let obj = ObjectFieldType jloName
     in MethodDescriptor (replicate arity obj) (TypeReturn obj)

moduleNameToQualifiedClassName :: ModuleName -> QualifiedClassName
moduleNameToQualifiedClassName (ModuleName name) =
    QualifiedClassName (PackageName $ init name) (parseClassName $ last name)

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
