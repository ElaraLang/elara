{-# LANGUAGE RecordWildCards #-}

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
module Elara.JVM.Emit (emitIRModule) where

import Effectful
import Effectful.State.Static.Local
import Elara.Data.Pretty (prettyToText)
import Elara.Data.Unique
import Elara.JVM.Emit.Operator (translateOperatorName)
import Elara.JVM.Emit.State
import Elara.JVM.Emit.Types (stringTypeName)
import Elara.JVM.IR as IR
import Elara.Logging
import JVM.Data.Abstract.Builder (ClassBuilder, addAccessFlag, addField, addMethod, runClassBuilder, setName, setSuperClass)
import JVM.Data.Abstract.Builder.Code (CodeBuilder, emit, newLabel, runCodeBuilder)
import JVM.Data.Abstract.ClassFile
import JVM.Data.Abstract.ClassFile.AccessFlags (ClassAccessFlag (..), FieldAccessFlag (FPublic), MethodAccessFlag (..))
import JVM.Data.Abstract.ClassFile.Field (ClassFileField (ClassFileField))
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.ConstantPool (BootstrapArgument (..), BootstrapMethod (..), MethodHandleEntry (..), MethodRef (..))
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Instruction as JVM
import JVM.Data.Abstract.Name
import JVM.Data.Abstract.Type
import JVM.Data.Abstract.Type qualified as JVM
import JVM.Data.Analyse.StackMap (calculateStackMapFrames)
import JVM.Data.JVMVersion
import JVM.Data.Raw.Types (U2)

-- | Emit an IR Module to a list of ClassFiles
emitIRModule :: StructuredDebug :> r => IR.Module -> Eff r [ClassFile]
emitIRModule (IR.Module moduleName classes) = do
    for classes $ fmap fst . runClassBuilder moduleName java8 . emitIRClass

-- | Emit a single IR Class to a ClassFile
emitIRClass :: (StructuredDebug :> r, ClassBuilder :> r) => IR.Class -> Eff r ()
emitIRClass (IR.Class className super fields methods constructors) = do
    setName className
    setSuperClass super
    addAccessFlag Public
    mapM_ emitIRField fields
    mapM_ (emitIRMethod className) methods
    mapM_ (emitConstructor className) constructors
    when (shouldAddMainMethod className methods) $
        emitJVMMainMethod className
  where
    shouldAddMainMethod :: QualifiedClassName -> [IR.Method] -> Bool
    shouldAddMainMethod (QualifiedClassName _ (ClassName "Main")) =
        any isElaraMain
    shouldAddMainMethod _ = const False

    isElaraMain :: IR.Method -> Bool
    isElaraMain (IR.Method name (MethodDescriptor _ (TypeReturn (ObjectFieldType ioCls))) _ _ True) =
        name == "main" && ioCls == "Elara.IO"
    isElaraMain _ = False

-- | Emit a field to the current class
emitIRField :: (StructuredDebug :> r, ClassBuilder :> r) => IR.Field -> Eff r ()
emitIRField (IR.Field fieldName fieldType) = do
    let
        fieldInfo =
            ClassFileField
                [FPublic]
                fieldName
                fieldType
                []
    addField fieldInfo

-- | Emit a method to the current class
emitIRMethod :: (StructuredDebug :> r, ClassBuilder :> r, HasCallStack) => QualifiedClassName -> IR.Method -> Eff r ()
emitIRMethod thisClassName (IR.Method methodName methodDescriptor methodArgs methodCode isStatic) = do
    let createState = if isStatic then createMethodCreationState else createInstanceMethodCreationState
        accessFlags = if isStatic then [MPublic, MStatic] else [MPublic]
        maxLocalsModifier = if isStatic then 0 else 1
    (methodCreationState, codeAttributes, instructions) <-
        runCodeBuilder $
            execState
                (createState (fst <$> methodArgs) thisClassName)
                (traverse emitIRBlock methodCode)
    let maxLocals = maxLocalsModifier + methodCreationState.maxLocalVariables
    addMethod $
        buildClassFileMethod
            (translateOperatorName methodName)
            accessFlags
            methodDescriptor
            maxLocals
            thisClassName
            codeAttributes
            instructions

-- | Create method creation state for instance methods
createInstanceMethodCreationState ::
    -- | Argument names
    [Unique Text] ->
    -- | Name of the class containing the method
    QualifiedClassName ->
    MethodCreationState
createInstanceMethodCreationState args thisName =
    MethodCreationState
        (fromList $ zip (KnownName <$> args) [1 ..])
        (fromIntegral (length args) + 1)
        thisName
        mempty

-- | Emit a constructor to the current class
emitConstructor :: (StructuredDebug :> r, ClassBuilder :> r) => QualifiedClassName -> IR.Constructor -> Eff r ()
emitConstructor className (IR.Constructor constructorDesc constructorArgs constructorCode) = do
    (emitState, codeAttributes, instructions) <-
        runCodeBuilder $
            execState
                (createInstanceMethodCreationState (fst <$> constructorArgs) className)
                (traverse emitIRBlock constructorCode)
    let maxLocals = 1 + emitState.maxLocalVariables
    addMethod $ buildClassFileMethod "<init>" [MPublic] constructorDesc maxLocals className codeAttributes instructions

-- | Build a ClassFileMethod with the standard Code attribute
buildClassFileMethod ::
    -- | Method name
    Text ->
    -- | Access flags
    [MethodAccessFlag] ->
    -- | Method descriptor
    MethodDescriptor ->
    -- | Max locals
    U2 ->
    -- | Class name (for stack map calculation)
    QualifiedClassName ->
    -- | Additional code attributes
    [CodeAttribute] ->
    -- | Body of the method
    [JVM.Instruction] ->
    ClassFileMethod
buildClassFileMethod name accessFlags desc maxLocals className codeAttrs instructions =
    ClassFileMethod
        { methodAccessFlags = accessFlags
        , methodName = name
        , methodDescriptor = desc
        , methodAttributes =
            fromList
                [ Code $
                    CodeAttributeData
                        { maxStack = 10 -- TODO: Calculate this properly
                        , maxLocals = maxLocals
                        , code = instructions
                        , exceptionTable = []
                        , codeAttributes = StackMapTable (calculateStackMapFrames className accessFlags desc instructions) : codeAttrs
                        }
                ]
        }

-- | Effects needed to emit JVM code to a method
type EmitCode r = (StructuredDebug :> r, CodeBuilder :> r, State MethodCreationState :> r)

-- \| Emit a block of IR instructions
emitIRBlock :: EmitCode r => IR.Block -> Eff r ()
emitIRBlock (IR.Block label instrs) = do
    label' <- getLabel label
    emit $ JVM.Label label'
    mapM_ emitIRInstruction instrs

-- | Emit a single IR instruction to JVM instructions
emitIRInstruction :: EmitCode r => IR.Instruction -> Eff r ()
emitIRInstruction instr = case instr of
    IR.Assign var t expr -> do
        emitExpr expr
        emit $ CheckCast (fieldTypeToClassInfoType t) -- ensure correct type, might not always be necessary
        allocation <- findLocalVariable var
        emit $ AStore allocation
    IR.Return Nothing -> emit JVM.Return
    IR.Return (Just e) -> do
        emitExpr e
        emit JVM.AReturn
    IR.IReturn e -> do
        emitExpr e
        emit JVM.IReturn
    IR.Jump label -> do
        label' <- getLabel label
        emit $ Goto label'
    IR.JumpIf cond trueLabel falseLabel -> do
        exprType <- emitExpr cond
        case exprType of
            Just (PrimitiveClassInfoType JVM.Boolean) -> do
                trueLabel' <- getLabel trueLabel
                falseLabel' <- getLabel falseLabel
                emit $ IfEq falseLabel'
                emit $ Goto trueLabel'
            Just (ClassInfoType "Elara.Prim.Bool") -> do
                trueLabel' <- getLabel trueLabel
                falseLabel' <- getLabel falseLabel
                emit $ Instanceof (ClassInfoType "Elara.Prim.True")
                emit $ IfEq falseLabel'
                emit $ Goto trueLabel'
            other -> error $ "emitIRInstruction: JumpIf condition has unsupported type: " <> prettyToText other
    IR.JumpIfPrimitiveBool cond trueLabel falseLabel -> do
        emitExpr cond
        trueLabel' <- getLabel trueLabel
        falseLabel' <- getLabel falseLabel
        emit $ IfEq falseLabel' -- since 0 = false
        emit $ Goto trueLabel'
    IR.ExprStmt expr -> void (emitExpr expr)
    IR.Super superName args -> do
        emit $ JVM.ALoad 0
        for_ args emitExpr
        let argTypes = replicate (length args) (ObjectFieldType "java/lang/Object") -- TODO: get proper types
        emit $
            JVM.InvokeSpecial
                (ClassInfoType superName)
                "<init>"
                (MethodDescriptor argTypes VoidReturn)
    IR.SetField fieldClass field fieldType val -> do
        emit $ JVM.ALoad 0 -- load 'this'
        emitExpr val
        emit $ JVM.PutField (ClassInfoType fieldClass) field fieldType

{- | Emit an expression onto the stack
Returns the type that will be on top of the stack after emitting the expression, if known
-}
emitExpr :: EmitCode r => IR.Expr -> Eff r (Maybe ClassInfoType)
emitExpr expr = case expr of
    IR.LitInt n -> do
        emit $ LDC (LDCInt $ fromInteger n) -- TODO: fromInteger bad
        emit $ JVM.InvokeStatic (ClassInfoType "java.lang.Integer") "valueOf" (MethodDescriptor [PrimitiveFieldType JVM.Int] (TypeReturn $ ObjectFieldType "java.lang.Integer"))
        pure $ Just (ClassInfoType "java.lang.Integer")
    IR.LitString s -> do
        emit $ JVM.New (ClassInfoType stringTypeName)
        emit JVM.Dup
        emit $ LDC (LDCString s)
        emit $ JVM.InvokeSpecial (ClassInfoType stringTypeName) "<init>" (MethodDescriptor [ObjectFieldType "java.lang.String"] VoidReturn)
        pure $ Just (ClassInfoType stringTypeName)
    IR.LitChar c -> do
        emit $ LDC (LDCInt $ fromEnum c)
        emit $ JVM.InvokeStatic (ClassInfoType "java.lang.Character") "valueOf" (MethodDescriptor [PrimitiveFieldType JVM.Char] (TypeReturn $ ObjectFieldType "java.lang.Character"))
        pure $ Just (ClassInfoType "java.lang.Character")
    IR.LitBool b -> emitElaraBool b
    IR.PrimitiveLitBool True -> emit JVM.IConst1 >> pure (Just (PrimitiveClassInfoType JVM.Boolean))
    IR.PrimitiveLitBool False -> emit JVM.IConst0 >> pure (Just (PrimitiveClassInfoType JVM.Boolean))
    IR.LitUnit -> do
        emit $ JVM.GetStatic (ClassInfoType "Elara.Unit") "unit" (ObjectFieldType "Elara.Unit")
        pure $ Just (ClassInfoType "Elara.Unit")
    IR.InstanceOf e t -> do
        emitExpr e
        emit $ Instanceof (fieldTypeToClassInfoType t)
        pure $ Just (PrimitiveClassInfoType JVM.Boolean)
    IR.FieldRef className fieldName fieldType -> do
        emit $ JVM.GetStatic (ClassInfoType className) fieldName fieldType
        pure $ Just (fieldTypeToClassInfoType fieldType)
    IR.LocalVar u t -> do
        allocation <- findLocalVariable u
        emit $ ALoad allocation
        pure $ Just (fieldTypeToClassInfoType t)
    IR.This _ -> do
        emit $ ALoad 0
        pure Nothing
    IR.GetField obj fieldClass fieldName fieldType -> do
        emitExpr obj
        emit $ JVM.CheckCast fieldClass
        emit $ JVM.GetField fieldClass fieldName fieldType
        pure $ Just (fieldTypeToClassInfoType fieldType)
    IR.Call f args -> do
        case f of
            IR.InvokeStatic cls name desc -> do
                for_ args emitExpr
                emit $ JVM.InvokeStatic (ClassInfoType cls) (translateOperatorName name) desc
                pure $ case desc of
                    MethodDescriptor _ (TypeReturn retType) -> Just (fieldTypeToClassInfoType retType)
                    _ -> Nothing
            IR.InvokeInterface target targetType name (MethodDescriptor argTys ret) -> do
                emitExpr target
                for_ args emitExpr
                let obj = ObjectFieldType "java.lang.Object"
                    argTypes = replicate (length argTys) obj
                    retType = obj
                    erasedDesc = MethodDescriptor argTypes (TypeReturn retType)
                emit $ JVM.InvokeInterface (ClassInfoType targetType) name erasedDesc

                case ret of
                    TypeReturn ft@(ObjectFieldType cls)
                        | cls /= "java.lang.Object" -> do
                            emit $ CheckCast (fieldTypeToClassInfoType ft)
                            pure $ Just (fieldTypeToClassInfoType ft)
                    _ -> pure Nothing
            IR.InvokeVirtual target targetType name desc@(MethodDescriptor _ ret) -> do
                emitExpr target
                for_ args emitExpr
                emit $ JVM.InvokeVirtual (ClassInfoType targetType) name desc

                case ret of
                    TypeReturn ft@(ObjectFieldType cls)
                        | cls /= "java.lang.Object" -> do
                            emit $ CheckCast (fieldTypeToClassInfoType ft)
                            pure $ Just (fieldTypeToClassInfoType ft)
                    _ -> pure Nothing
    IR.BinaryOp op lhs rhs -> do
        emitExpr lhs
        emitExpr rhs
        case op of
            IR.Equals -> do
                emit $
                    JVM.InvokeStatic
                        (ClassInfoType "java.util.Objects")
                        "equals"
                        ( MethodDescriptor
                            [ObjectFieldType "java.lang.Object", ObjectFieldType "java.lang.Object"]
                            (TypeReturn $ PrimitiveFieldType JVM.Boolean)
                        )
                primitiveBooleanToElaraBoolean
            _ -> error $ "emitExpr: Unhandled binary operator: " <> prettyToText op
    IR.MakeClosure{..} -> do
        -- Emit captured values onto the stack
        for_ capturedValues (emitExpr . fst)

        let capturedTypes = map snd capturedValues
        let MethodDescriptor originalArgTypes returnType = closureTarget
        let capturedCount = length capturedValues
        let remainingArgTypes = drop capturedCount originalArgTypes

        let implMethodHandle =
                MHInvokeStatic $
                    MethodRef
                        (ClassInfoType closureTargetClass)
                        (translateOperatorName closureTargetMethod)
                        closureTarget

        emitInvokeDynamic capturedTypes remainingArgTypes returnType implMethodHandle closureInterface
    IR.MakeConstructorClosure{..} -> do
        -- Emit captured values onto the stack
        for_ ctorCapturedValues (emitExpr . fst)

        let capturedTypes = map snd ctorCapturedValues
        let MethodDescriptor originalArgTypes _ = ctorClosureDesc
        let capturedCount = length ctorCapturedValues
        let remainingArgTypes = drop capturedCount originalArgTypes
        let returnType = TypeReturn (ObjectFieldType ctorClosureClass)

        let implMethodHandle =
                MHNewInvokeSpecial $
                    MethodRef
                        (ClassInfoType ctorClosureClass)
                        "<init>"
                        ctorClosureDesc

        emitInvokeDynamic capturedTypes remainingArgTypes returnType implMethodHandle ctorClosureInterface
    IR.PrimOp op args -> do
        case (op, args) of
            (IR.UndefinedError, [msg]) -> do
                emitExpr msg
                emit $
                    JVM.InvokeStatic
                        (ClassInfoType "Elara.Error")
                        "undefined"
                        (MethodDescriptor [] (TypeReturn $ ObjectFieldType "java/lang/Object"))

                pure $ Just (ClassInfoType "java/lang/Object")
            (IR.PatternMatchFailedError, []) -> do
                emit $
                    JVM.InvokeStatic
                        (ClassInfoType "Elara.Error")
                        "patternMatchFail"
                        (MethodDescriptor [] (TypeReturn $ ObjectFieldType "java/lang/Object"))
                pure $ Just (ClassInfoType "java/lang/Object")
            (IR.IntAdd, [a, b]) -> emitBinaryIntOp "add" a b
            (IR.IntSubtract, [a, b]) -> emitBinaryIntOp "minus" a b
            (IR.IntMultiply, [a, b]) -> emitBinaryIntOp "times" a b
            (IR.IntNegate, [a]) -> do
                -- Get the curried function: Prelude.negate : Int -> Int
                emit $ GetStatic (ClassInfoType "Elara.Prelude") "negate" (ObjectFieldType "Elara.Func")
                emitExpr a
                emit $ JVM.InvokeInterface (ClassInfoType "Elara.Func") "run" (MethodDescriptor [javaObject] (TypeReturn javaObject))
                emit $ CheckCast (ClassInfoType "java.lang.Integer")
                pure $ Just (ClassInfoType "java.lang.Integer")
            (IR.Println, [msg]) -> do
                emitExpr msg
                emit $ JVM.InvokeStatic (ClassInfoType "Elara.IO") "println" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "Elara.IO")))
                pure $ Just (ClassInfoType "Elara.IO")
            (IR.StringHead, [str]) -> emitStringMethod str "head" [] "java.lang.Character"
            (IR.StringIsEmpty, [str]) -> do
                emitExpr str
                emit $ JVM.InvokeVirtual (ClassInfoType stringTypeName) "isEmpty" (MethodDescriptor [] (TypeReturn $ PrimitiveFieldType JVM.Boolean))
                primitiveBooleanToElaraBoolean
            (IR.StringTail, [str]) -> emitStringMethod str "tail" [] stringTypeName
            (IR.StringCons, [ch, str]) -> do
                emitExpr str -- receiver
                emitExpr ch -- argument
                emit $ JVM.InvokeVirtual (ClassInfoType stringTypeName) "cons" (MethodDescriptor [ObjectFieldType "java.lang.Character"] (TypeReturn $ ObjectFieldType stringTypeName))
                pure $ Just (ClassInfoType stringTypeName)
            (IR.ToString, [obj]) -> do
                emitExpr obj
                emit $ JVM.InvokeStatic (ClassInfoType "Elara.PrimOps") "toString" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn $ ObjectFieldType stringTypeName))
                pure $ Just (ClassInfoType stringTypeName)
            (IR.PrimEquals, [a, b]) -> do
                emitExpr a
                emitExpr b
                emit $ JVM.InvokeStatic (ClassInfoType "java.util.Objects") "equals" objectsEqualsDesc
                primitiveBooleanToElaraBoolean
            (IR.PrimCompare, [a, b]) -> do
                emitExpr a
                emitExpr b
                emit $ JVM.InvokeStatic (ClassInfoType "Elara.PrimOps") "compare" (MethodDescriptor [javaObject, javaObject] (TypeReturn $ ObjectFieldType "java.lang.Integer"))
                pure $ Just (ClassInfoType "java.lang.Integer")
            (IR.IOBind, [io, func]) -> do
                emitExpr io
                emitExpr func
                emit $ JVM.InvokeVirtual (ClassInfoType "Elara.IO") "bind" (MethodDescriptor [ObjectFieldType "Elara.Func"] (TypeReturn $ ObjectFieldType "Elara.IO"))
                pure $ Just (ClassInfoType "Elara.IO")
            (IR.DebugWithMsg, [msg, value]) -> do
                emitExpr msg
                emitExpr value
                emit $ JVM.InvokeStatic (ClassInfoType "Elara.PrimOps") "debugWithMsg" (MethodDescriptor [ObjectFieldType stringTypeName, javaObject] (TypeReturn javaObject))
                pure $ Just (ClassInfoType "java.lang.Object")
            (IR.ThrowError, [msg]) -> do
                emitExpr msg
                emit $ JVM.InvokeStatic (ClassInfoType "Elara.PrimOps") "toString" (MethodDescriptor [javaObject] (TypeReturn $ ObjectFieldType stringTypeName))
                emit $ JVM.InvokeStatic (ClassInfoType "Elara.Error") "throwError" (MethodDescriptor [ObjectFieldType stringTypeName] (TypeReturn javaObject))
                pure $ Just (ClassInfoType "java/lang/Object")
            other -> error $ "emitExpr: Unhandled primitive operation: " <> prettyToText other
    IR.New className args -> do
        emit $ JVM.New (ClassInfoType className)
        emit JVM.Dup
        for_ args (emitExpr . fst)
        let argTypes = map snd args
        emit $ JVM.InvokeSpecial (ClassInfoType className) "<init>" (MethodDescriptor argTypes VoidReturn)
        pure $ Just (ClassInfoType className)
    IR.Cast expr targetType -> do
        emitExpr expr
        emit $ JVM.CheckCast (fieldTypeToClassInfoType targetType)
        pure $ Just (fieldTypeToClassInfoType targetType)
    _ -> error $ "emitExpr: Unhandled expression: " <> prettyToText expr

-- | Common type aliases for Java types
javaObject :: FieldType
javaObject = ObjectFieldType "java.lang.Object"

objectsEqualsDesc :: MethodDescriptor
objectsEqualsDesc = MethodDescriptor [javaObject, javaObject] (TypeReturn $ PrimitiveFieldType JVM.Boolean)

-- | Emit a binary integer operation using a curried Prelude function
emitBinaryIntOp :: EmitCode r => Text -> IR.Expr -> IR.Expr -> Eff r (Maybe ClassInfoType)
emitBinaryIntOp funcName a b = do
    -- Get the curried function: Prelude.funcName : Int -> Int -> Int
    emit $ GetStatic (ClassInfoType "Elara.Prelude") funcName (ObjectFieldType "Elara.Func")
    -- Apply first argument: stack is [func], need [func, a] -> [add_a]
    emitExpr a
    emit $ JVM.InvokeInterface (ClassInfoType "Elara.Func") "run" (MethodDescriptor [javaObject] (TypeReturn javaObject))
    -- Apply second argument: stack is [add_a], need [add_a, b] -> [result]
    emitExpr b
    emit $ JVM.InvokeInterface (ClassInfoType "Elara.Func") "run" (MethodDescriptor [javaObject] (TypeReturn javaObject))
    emit $ CheckCast (ClassInfoType "java.lang.Integer")
    pure $ Just (ClassInfoType "java.lang.Integer")

-- | Emit a method call on Elara.String
emitStringMethod :: EmitCode r => IR.Expr -> Text -> [FieldType] -> QualifiedClassName -> Eff r (Maybe ClassInfoType)
emitStringMethod receiver methodName argTypes retTypeName = do
    emitExpr receiver
    emit $ JVM.InvokeVirtual (ClassInfoType stringTypeName) methodName (MethodDescriptor argTypes (TypeReturn $ ObjectFieldType retTypeName))
    pure $ Just (ClassInfoType retTypeName)

-- | Emit invokedynamic for closure creation (shared between MakeClosure and MakeConstructorClosure)
emitInvokeDynamic ::
    EmitCode r =>
    -- | captured types
    [FieldType] ->
    -- | remaining arg types
    [FieldType] ->
    -- | return type
    ReturnDescriptor ->
    -- | implementation method handle
    MethodHandleEntry ->
    -- | target interface
    QualifiedClassName ->
    Eff r (Maybe ClassInfoType)
emitInvokeDynamic capturedTypes remainingArgTypes returnType implMethodHandle targetInterface = do
    let specializedDesc = MethodDescriptor remainingArgTypes returnType
    let erasedDesc = MethodDescriptor (replicate (length remainingArgTypes) javaObject) (TypeReturn javaObject)

    let bootstrapMethodHandle =
            MHInvokeStatic $
                MethodRef
                    (ClassInfoType "java/lang/invoke/LambdaMetafactory")
                    "metafactory"
                    ( MethodDescriptor
                        [ ObjectFieldType "java/lang/invoke/MethodHandles$Lookup"
                        , ObjectFieldType "java/lang/String"
                        , ObjectFieldType "java/lang/invoke/MethodType"
                        , ObjectFieldType "java/lang/invoke/MethodType"
                        , ObjectFieldType "java/lang/invoke/MethodHandle"
                        , ObjectFieldType "java/lang/invoke/MethodType"
                        ]
                        (TypeReturn $ ObjectFieldType "java/lang/invoke/CallSite")
                    )

    let bootstrapMethod =
            BootstrapMethod
                bootstrapMethodHandle
                [ BMMethodArg erasedDesc
                , BMMethodHandleArg implMethodHandle
                , BMMethodArg specializedDesc
                ]

    let invokedTypeDesc = MethodDescriptor capturedTypes (TypeReturn (ObjectFieldType targetInterface))
    emit $ InvokeDynamic bootstrapMethod "run" invokedTypeDesc
    pure $ Just (ClassInfoType targetInterface)

emitJVMMainMethod :: (StructuredDebug :> r, ClassBuilder :> r) => QualifiedClassName -> Eff r ()
emitJVMMainMethod thisClassName = do
    let elaraIOClass :: QualifiedClassName
        elaraIOClass = "Elara/IO"

        -- elara Main.main() : Elara.IO
        elaraMainDesc =
            MethodDescriptor [] (TypeReturn (ObjectFieldType elaraIOClass))

        javaMainDesc =
            MethodDescriptor
                [ArrayFieldType (ObjectFieldType "java/lang/String")]
                VoidReturn

    (_, codeAttributes, instructions) <-
        runCodeBuilder $ do
            -- Call Elara Main.main() : Elara.IO
            emit $
                JVM.InvokeStatic
                    (ClassInfoType thisClassName)
                    "main"
                    elaraMainDesc

            -- Call IO#run() : void
            emit $
                JVM.InvokeVirtual
                    (ClassInfoType elaraIOClass)
                    "run"
                    (MethodDescriptor [] VoidReturn)

            emit JVM.Return

    let methodInfo =
            ClassFileMethod
                { methodAccessFlags = [MPublic, MStatic]
                , methodName = "main"
                , methodDescriptor = javaMainDesc
                , methodAttributes =
                    fromList
                        [ Code $
                            CodeAttributeData
                                { maxStack = 2 -- IO on stack, then call run
                                , maxLocals = 1 -- String[] args (unused)
                                , code = instructions
                                , exceptionTable = []
                                , codeAttributes = StackMapTable (calculateStackMapFrames thisClassName [MPublic, MStatic] javaMainDesc instructions) : codeAttributes
                                }
                        ]
                }

    addMethod methodInfo

-- | Emit an Elara boolean (True or False) onto the stack
emitElaraBool :: EmitCode r => Bool -> Eff r (Maybe ClassInfoType)
emitElaraBool b = do
    let className = if b then "Elara.Prim.True" else "Elara.Prim.False"
    emit $ JVM.New (ClassInfoType className)
    emit JVM.Dup
    emit $ JVM.InvokeSpecial (ClassInfoType className) "<init>" (MethodDescriptor [] VoidReturn)
    emit $ CheckCast (ClassInfoType "Elara.Prim.Bool")
    pure $ Just (ClassInfoType "Elara.Prim.Bool")

-- | Convert a primitive boolean (int 0 or 1) on the stack to an Elara.Prim.Bool
primitiveBooleanToElaraBoolean :: EmitCode r => Eff r (Maybe ClassInfoType)
primitiveBooleanToElaraBoolean = do
    falseLabel <- newLabel
    endLabel <- newLabel
    emit $ IfEq falseLabel
    -- true case
    void $ emitElaraBool True
    emit $ Goto endLabel
    -- false case
    emit $ JVM.Label falseLabel
    void $ emitElaraBool False
    emit $ JVM.Label endLabel
    pure $ Just (ClassInfoType "Elara.Prim.Bool")
