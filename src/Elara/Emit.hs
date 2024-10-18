{- | Emits JVM bytecode from the Core AST

The emitting process:
Every module is translated to a class file.
For each declaration, we turn it into a field if it is a value (i.e. a zero argument function, including IO actions),
or a method if it is a function.

*Translation of functions to methods:*

The emitter will eta expand declarations:
@ let id = \x -> x@
will be translated to:
@ public static Object id(Object x) { return x; }@

however it does not do any complex analysis on the code:
@ let add x =
    if x == 0 then \y -> y else \y -> x + y
@
The higher order function here _can_ be avoided if we rearrange the code into:
@ let add = \x -> \y ->
    if x == 0 then y else x + y
@
but this responsibility is left to the CoreToCore pass, so the emitter will still produce inefficient code:

@ public static Func add(int x) {
    if (x == 0) {
        return (y) -> y;
    } else {
        return (y) -> x + y;
    }
}
@

What this means is that the emitted method's arity will always match the declared arity of the function
(i.e. how many directly nested lambdas there are)
-}
module Elara.Emit where

import Elara.AST.Name
import Elara.AST.VarRef
import Elara.Core as Core
import Elara.Core.Module
import Elara.Data.Pretty (prettyToText)
import Elara.Emit.Utils (createModuleName, generateFieldType)
import Elara.Logging
import Elara.Prim.Core
import JVM.Data.Abstract.Builder (ClassBuilder, addField, addMethod, runClassBuilder)
import JVM.Data.Abstract.Builder.Code (CodeBuilder, emit', runCodeBuilder)
import JVM.Data.Abstract.ClassFile
import JVM.Data.Abstract.ClassFile.Field (ClassFileField (ClassFileField), ConstantValue (ConstantInteger), FieldAttribute (ConstantValue))
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Type
import JVM.Data.Abstract.Type qualified as JVM
import JVM.Data.JVMVersion
import JVM.Data.Pretty (showPretty)
import JVM.Data.Raw.ClassFile (Attribute (CodeAttribute))
import Polysemy

emitCoreModule :: Member StructuredDebug r => CoreModule -> Sem r ClassFile
emitCoreModule (CoreModule name decls) = do
    (clf, _) <- runClassBuilder (createModuleName name) java8 $ for_ decls $ \decl -> do
        emitCoreDecl decl
        pass

    pure clf

emitCoreDecl :: (Member ClassBuilder r, Member StructuredDebug r) => CoreDeclaration -> Sem r ()
emitCoreDecl decl = case decl of
    CoreValue (NonRecursive (n@(Id name type' _), e)) -> do
        let declName = runIdentity (varRefVal name)
        case e of
            Core.Lit (Core.Int i) -> do
                addField $ ClassFileField [] declName (ObjectFieldType "java.lang.Integer") [ConstantValue (ConstantInteger (fromIntegral i))]
            e -> do
                (_, attrs, code) <- runCodeBuilder (emitCoreExpr e)
                addMethod $ ClassFileMethod [] declName (MethodDescriptor [] (TypeReturn (ObjectFieldType "java.lang.Object"))) (fromList [Code $ CodeAttributeData 100 100 code [] attrs])
        pass
    _ -> pass

emitCoreExpr e = case e of
    Core.Lit s -> do
        emitValue s
    (App ((Var ((Id (Global' (Qualified "elaraPrimitive" _)) _ _)))) (Lit (String "println"))) -> do
        emit'
            [ InvokeStatic (ClassInfoType "Elara.IO") "println" (MethodDescriptor [ObjectFieldType "java.lang.String"] (TypeReturn (ObjectFieldType "Elara.IO")))
            ]
    (App ((Var ((Id (Global' (Qualified "elaraPrimitive" _)) _ _)))) (Lit (String "toString"))) -> do
        emit'
            [ InvokeVirtual (ClassInfoType "java.lang.Object") "toString" (MethodDescriptor [] (TypeReturn (ObjectFieldType "java.lang.String")))
            ]
    v@(Var ((Id (Global' qn@(Qualified n mn)) t _))) -> emit' [GetStatic (ClassInfoType $ createModuleName mn) n (generateFieldType t)]
    Core.App f x -> do
        emitCoreExpr x
        emitCoreExpr f
    other -> pass

emitValue :: Member CodeBuilder r => Literal -> Sem r ()
emitValue s = case s of
    Core.Int i ->
        emit'
            [ LDC (LDCInt (fromIntegral i))
            , InvokeStatic (ClassInfoType "java.lang.Integer") "valueOf" (MethodDescriptor [PrimitiveFieldType JVM.Int] (TypeReturn (ObjectFieldType "java.lang.Integer")))
            ]
    Core.String s ->
        emit'
            [ LDC (LDCString s)
            ]
    _ -> error "Not implemented"
