{- | This module contains the code for emitting the ADT for the Elara language.
The representation for these types is fairly simple, as at the moment we're not gonna worry about
using things like sealed classes.

Given a data type
@ type Option a = Some a | None @

we generate (the equivalent of) the following code:

>public interface Option<A> {
>   <R> R match(Func<Some<A>, R> some, Func<None, R> none);
>
>   static <A> Option<A> some(A value) {
>       return new Some<>(value);
>   }
>
>   static <A> Option<A> none() {
>       return new None<>();
>   }
>
>   public final class Some<A> implements Option<A> {
>       private final A value;
>
>       private Some(A value) {
>           this.value = value;
>       }
>
>       public A getValue() {
>           return value;
>       }
>
>       @Override
>       public <R> R match(Func<Some<A>, R> some, Func<None, R> none) {
>           return some.run(this);
>       }
>
>       // equals, hashCode, toString
>   }
>
>   public final class None<A> implements Option<A> {
>       private None() {}
>
>       @Override
>       public <R> R match(Func<Some<A>, R> some, Func<None, R> none) {
>           return none.run(this);
>       }
>
>       // equals, hashCode, toString
>   }
>

We use a sort of visitor pattern to allow for pattern matching on the ADT, which
means code using pattern matching can be expressed fairly cleanly without generating a lot of
/instanceof/ checks every time.

However in the future I'll probably end up changing this to avoid boxing behaviour into lambdas all the time
-}
module Elara.Emit.ADT where

import Elara.Core.Module (CoreTypeDecl (..), CoreTypeDeclBody (..))

import Elara.AST.Name (unqualified)
import Elara.Core (DataCon (..), functionTypeArgs)
import Elara.Data.Unique (makeUnique)
import Elara.Emit.Method (createMethodWithCodeBuilder)
import Elara.Emit.Monad (InnerEmit, addClass, addInnerClass)
import Elara.Emit.State (MethodCreationState (maxLocalVariables), findLocalVariable)
import Elara.Emit.Utils (createQualifiedClassName, createQualifiedInnerClassName, generateFieldType)
import Elara.Parse.Type (functionType)
import JVM.Data.Abstract.Builder
import JVM.Data.Abstract.Builder.Code
import JVM.Data.Abstract.ClassFile.AccessFlags
import JVM.Data.Abstract.ClassFile.Field
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Type
import Polysemy
import Relude.Unsafe ((!!))

generateADTClasses :: InnerEmit r => CoreTypeDecl -> Sem r ()
generateADTClasses (CoreTypeDecl _ _ _ (CoreTypeAlias _)) = pass -- nothing to generate for type aliases
generateADTClasses (CoreTypeDecl name kind tvs (CoreDataDecl ctors)) = do
    let typeClassName = createQualifiedClassName name
    addClass typeClassName $ do
        addAccessFlag Public
        addAccessFlag Abstract
        let matchSig =
                MethodDescriptor (ObjectFieldType "Elara/Func" <$ ctors) (TypeReturn $ ObjectFieldType "java/lang/Object")

        -- add boring empty constructor
        createMethodWithCodeBuilder typeClassName (MethodDescriptor [] VoidReturn) [MProtected] "<init>" $ do
            emit $ ALoad 0
            emit $ InvokeSpecial (ClassInfoType "java/lang/Object") "<init>" (MethodDescriptor [] VoidReturn)

        addMethod $
            ClassFileMethod
                [MPublic, MAbstract]
                "match"
                matchSig
                mempty
        for_ ctors $ \(DataCon ctorName ctorType conTy) -> do
            let innerConClassName = createQualifiedInnerClassName (ctorName ^. unqualified) typeClassName
            let fields = functionTypeArgs ctorType
            -- Create static factory method
            createMethodWithCodeBuilder typeClassName (MethodDescriptor (generateFieldType <$> fields) (TypeReturn $ ObjectFieldType typeClassName)) [MPublic, MStatic] ("_" <> ctorName ^. unqualified) $ do
                emit $ New (ClassInfoType innerConClassName)
                emit Dup
                for_ (zip fields [0 ..]) $ \(_, i) -> do
                    emit $ ALoad (fromIntegral i)
                emit $ InvokeSpecial (ClassInfoType innerConClassName) "<init>" (MethodDescriptor (generateFieldType <$> fields) VoidReturn)

            -- Create inner class
            addInnerClass (ctorName ^. unqualified) $ do
                addAccessFlag Public
                addAccessFlag Final
                addAccessFlag Super
                setSuperClass typeClassName

                for_ (zip fields [0 ..]) $ \(field, i) -> do
                    addField $ ClassFileField [FPrivate, FFinal] ("field" <> show i) (generateFieldType field) []

                thisName <- getName
                createMethodWithCodeBuilder thisName (MethodDescriptor (generateFieldType <$> fields) VoidReturn) [MPublic] "<init>" $ do
                    -- call super constructor
                    emit $ ALoad 0
                    emit $ InvokeSpecial (ClassInfoType typeClassName) "<init>" (MethodDescriptor [] VoidReturn)
                    emit $ ALoad 0
                    for_ (zip fields [0 ..]) $ \(field, i) -> do
                        emit $ ALoad (fromIntegral i + 1)
                        emit $ PutField (ClassInfoType thisName) ("field" <> show i) (generateFieldType field)

                -- generate toString
                createMethodWithCodeBuilder thisName (MethodDescriptor [] (TypeReturn $ ObjectFieldType "java/lang/String")) [MPublic] "toString" $ do
                    emit $ New (ClassInfoType "java/lang/StringBuilder")
                    emit Dup
                    emit $ LDC (LDCString $ ctorName ^. unqualified)
                    emit $ InvokeSpecial (ClassInfoType "java/lang/StringBuilder") "<init>" (MethodDescriptor [ObjectFieldType "java/lang/String"] VoidReturn)

                    for_ (zip fields [0 ..]) $ \(field, i) -> do
                        -- add whitespace
                        emit $ LDC (LDCString " ")
                        emit $ InvokeVirtual (ClassInfoType "java/lang/StringBuilder") "append" (MethodDescriptor [ObjectFieldType "java/lang/String"] (TypeReturn $ ObjectFieldType "java/lang/StringBuilder"))
                        -- get the field
                        emit $ ALoad 0
                        emit $ GetField (ClassInfoType thisName) ("field" <> show i) (generateFieldType field)
                        -- toString it
                        emit $ InvokeVirtual (ClassInfoType "java/lang/Object") "toString" (MethodDescriptor [] (TypeReturn $ ObjectFieldType "java/lang/String"))
                        -- append it to the StringBuilder
                        emit $ InvokeVirtual (ClassInfoType "java/lang/StringBuilder") "append" (MethodDescriptor [generateFieldType field] (TypeReturn $ ObjectFieldType "java/lang/StringBuilder"))
                    emit $ InvokeVirtual (ClassInfoType "java/lang/StringBuilder") "toString" (MethodDescriptor [] (TypeReturn $ ObjectFieldType "java/lang/String"))