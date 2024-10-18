module Elara.Emit where

import Elara.AST.Name
import Elara.AST.VarRef
import Elara.Core as Core
import Elara.Core.Module
import Elara.Data.Pretty (prettyToText)
import Elara.Emit.Utils (createModuleName)
import Elara.Logging
import Elara.Prim.Core
import JVM.Data.Abstract.Builder (ClassBuilder, addField, runClassBuilder)
import JVM.Data.Abstract.Builder.Code (CodeBuilder, emit', runCodeBuilder)
import JVM.Data.Abstract.ClassFile
import JVM.Data.Abstract.ClassFile.Field (ClassFileField (ClassFileField), ConstantValue (ConstantInteger), FieldAttribute (ConstantValue))
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Type
import JVM.Data.Abstract.Type qualified as JVM
import JVM.Data.JVMVersion
import JVM.Data.Pretty (showPretty)
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
                code <- runCodeBuilder (emitCoreExpr e)
                pass
        pass
    _ -> pass

emitCoreExpr e = case e of
    Core.Lit s -> do
        emitValue s
    (App ((Var ((Id (Global' (Qualified "elaraPrimitive" _)) _ _)))) (Lit (String "println"))) -> do
        emit'
            [ InvokeStatic (ClassInfoType "Elara.IO") "println" (MethodDescriptor [ObjectFieldType "java.lang.String"] (TypeReturn (ObjectFieldType "Elara.IO")))
            ]
    Core.App f x -> do
        emitCoreExpr f
        emitCoreExpr x
    _ -> pass

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
