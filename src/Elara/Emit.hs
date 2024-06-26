{-# LANGUAGE OverloadedLists #-}

{- | Emits JVM bytecode from Elara AST.
Conventions:
* We always uncurry functions, so a function of type a -> b -> c is compiled to a JVM function c f(a, b).
This means that currying can be avoided in a lot of cases (through arity analysis),
  skipping the overhead of creating a lambda and calling repeated invoke() functions
-}
module Elara.Emit where

import Data.Generics.Product
import Elara.AST.Name (ModuleName (..))
import Elara.AST.VarRef (varRefVal)
import Elara.Core (Bind (..), Expr (Var), Type (..), Var (..))
import Elara.Core.Module (CoreDeclaration (..), CoreModule)
import Elara.Core.Pretty ()
import Elara.Data.Pretty
import Elara.Data.TopologicalGraph (TopologicalGraph, traverseGraphRevTopologically_)
import Elara.Data.Unique
import Elara.Emit.ADT (generateADTClasses)
import Elara.Emit.Error
import Elara.Emit.Expr
import Elara.Emit.Lambda (etaExpandN)
import Elara.Emit.Method (createMethod, createMethodWith, createMethodWithCodeBuilder, etaExpandNIntoMethod)
import Elara.Emit.Method.Descriptor
import Elara.Emit.Monad
import Elara.Emit.Operator (translateOperatorName)
import Elara.Emit.Params
import Elara.Emit.State (MethodCreationState, initialMethodCreationState)
import Elara.Emit.Utils
import Elara.Emit.Var (JVMBinder (..), JVMExpr, transformTopLevelJVMLambdas, transformTopLevelLambdas)
import Elara.Error (DiagnosticWriter, runErrorOrReport)
import Elara.ToCore (stripForAll)
import JVM.Data.Abstract.Builder
import JVM.Data.Abstract.Builder.Code hiding (code)
import JVM.Data.Abstract.ClassFile
import JVM.Data.Abstract.ClassFile.AccessFlags
import JVM.Data.Abstract.ClassFile.Field
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Instruction (Instruction, Instruction' (..))
import JVM.Data.Abstract.Name (QualifiedClassName)
import JVM.Data.Abstract.Type as JVM (ClassInfoType (ClassInfoType), FieldType (ArrayFieldType, ObjectFieldType))
import JVM.Data.JVMVersion
import Polysemy
import Polysemy.Error
import Polysemy.Log (Log)
import Polysemy.Log qualified as Log
import Polysemy.Maybe
import Polysemy.Reader
import Polysemy.State
import Polysemy.Writer (runWriter, tell)
import Print (showPretty)

liftState :: Member (State CLInitState) r => Sem (State MethodCreationState : r) a -> Sem r a
liftState act = do
    (CLInitState s) <- get @CLInitState
    (s', x) <- runState s act
    put $ CLInitState s'
    pure x

emitGraph :: forall r. Emit r => TopologicalGraph CoreModule -> Sem r [(ModuleName, [ClassFile])]
emitGraph g = do
    let tellMod = emitModule >=> tell . one
    fst <$> runWriter (traverseGraphRevTopologically_ tellMod g)

runInnerEmit ::
    ( Member (Embed IO) r
    , Member (DiagnosticWriter (Doc AnsiStyle)) r
    , Member MaybeE r
    ) =>
    QualifiedClassName ->
    i ->
    Sem (Error EmitError : UniqueGen : State CLInitState : Reader GenParams : Reader i : Reader QualifiedClassName : CodeBuilder : r) a ->
    Sem r (a, CLInitState, [CodeAttribute], [Instruction])
runInnerEmit name version x = do
    ((clinitState, a), attrs, inst) <-
        runCodeBuilder
            . runReader name
            . runReader version
            . runReader defaultGenParams
            . runState @CLInitState (CLInitState (initialMethodCreationState name))
            . uniqueGenToIO
            . runErrorOrReport
            $ x
    pure (a, clinitState, attrs, inst)

emitModule :: forall r. Emit r => CoreModule -> Sem r (ModuleName, [ClassFile])
emitModule m = fmap swap $ runMultiClassBuilder $ do
    Log.debug $ "Emitting module " <> showPretty (m ^. field @"name") <> "..."
    let name = createModuleName (m ^. field' @"name")
    version <- ask @JVMVersion
    addClass name $ do
        addAccessFlag Public
        (_, clinitState, attrs, clinit) <-
            runInnerEmit name version $
                addDeclarationsAndMain m

        runReader defaultGenParams $ addClinit clinitState attrs clinit

    pure
        ( m ^. field @"name"
        )

addDeclarationsAndMain :: (InnerEmit r, Member CodeBuilder r, Member (Reader GenParams) r) => CoreModule -> Sem r ()
addDeclarationsAndMain m = do
    traverse_ addDeclaration (m ^. field @"declarations")
    when (isMainModule m) (addMethod (generateMainMethod m))

addClinit :: (Member ClassBuilder r, Member (Reader GenParams) r) => CLInitState -> [CodeAttribute] -> [Instruction] -> Sem r ()
addClinit (CLInitState s) attrs = createMethodWith (MethodDescriptor [] VoidReturn) [MPublic, MStatic] "<clinit>" attrs s

addDeclaration :: (HasCallStack, InnerEmit r, Member CodeBuilder r, Member (Reader GenParams) r) => CoreDeclaration -> Sem r ()
addDeclaration declBody = case declBody of
    CoreValue (NonRecursive (n@(Id name type' _), e)) -> do
        Log.debug $ "Emitting non-recursive declaration " <> showPretty name <> ", with type " <> showPretty type' <> "..."
        let declName = translateOperatorName $ runIdentity (varRefVal name)
        if typeIsValue type'
            then do
                let fieldType = generateFieldType type'
                let field = ClassFileField [FPublic, FStatic] declName fieldType []
                Log.debug $ "Creating field " <> showPretty declName <> " of type " <> showPretty fieldType <> "..."
                addField field
                let e' = transformTopLevelLambdas e
                addStaticFieldInitialiser field e'
            else do
                descriptor <- generateNamedMethodDescriptor type' e
                -- Whenever we have a function declaration we do 2 things
                -- Turn it into a hidden method _name, doing the actual logic
                -- Create a getter method name, which just returns _name wrapped into a Func
                case stripForAll type' of
                    FuncTy{} -> do
                        Log.debug $ "Creating method " <> showPretty declName <> " with signature " <> showPretty descriptor <> "..."
                        thisName <- ask @QualifiedClassName
                        y <- transformTopLevelJVMLambdas <$> etaExpandNIntoMethod e type' thisName
                        Log.debug $ "Transformed lambda expression: " <> showPretty y
                        createMethod thisName descriptor ("_" <> declName) y
                        let getterDescriptor = NamedMethodDescriptor [] (TypeReturn (ObjectFieldType "Elara.Func"))
                        Log.debug $ "Creating getter method " <> showPretty declName <> " with signature " <> showPretty getterDescriptor <> "..."
                        createMethodWithCodeBuilder thisName getterDescriptor [MPublic, MStatic] declName $ do
                            Log.debug $ "Getting static field " <> showPretty declName <> "..."
                            inst <- etaExpandN (Var $ Normal n) type' thisName
                            emit' inst
                            Log.debug $ "Returning static field " <> showPretty declName <> "..."
                        Log.debug "=="
                    _ -> do
                        Log.debug $ "Creating method " <> showPretty declName <> " with signature " <> showPretty descriptor <> "..."
                        let y = transformTopLevelLambdas e
                        Log.debug $ "Transformed lambda expression: " <> showPretty y
                        thisName <- ask @QualifiedClassName
                        createMethod thisName descriptor declName y
        Log.debug $ "Emitted non-recursive declaration " <> showPretty name
    CoreType decl -> do
        generateADTClasses decl
    _ -> undefined

isMainModule :: CoreModule -> Bool
isMainModule m = m ^. field @"name" == ModuleName ("Main" :| [])

-- | Adds an initialiser for a static field to <clinit>
addStaticFieldInitialiser :: (HasCallStack, InnerEmit r, Member CodeBuilder r, Member (Reader GenParams) r) => ClassFileField -> JVMExpr -> Sem r ()
addStaticFieldInitialiser (ClassFileField _ name fieldType _) e = do
    Log.debug $ "Adding initialiser for field " <> showPretty name <> " of type " <> showPretty fieldType <> "..."
    liftState $ generateInstructions e

    cn <- ask @QualifiedClassName
    emit $ PutStatic (ClassInfoType cn) name fieldType
    Log.debug $ "Added initialiser for field " <> showPretty name <> " of type " <> showPretty fieldType <> "."

-- | Generates a main method, which merely loads a IO action field called main and runs it
generateMainMethod :: CoreModule -> ClassFileMethod
generateMainMethod m =
    ClassFileMethod
        [MPublic, MStatic]
        "main"
        ( MethodDescriptor
            [ArrayFieldType (ObjectFieldType "java.lang.String")]
            VoidReturn
        )
        [ Code $
            CodeAttributeData
                { maxStack = 1 -- TODO: calculate this
                , maxLocals = 1 -- TODO: calculate this too
                , code =
                    [ GetStatic (ClassInfoType (createModuleName (m ^. field @"name"))) "main" (ObjectFieldType "Elara.IO")
                    , InvokeVirtual (ClassInfoType "Elara.IO") "run" (MethodDescriptor [] VoidReturn)
                    , Return
                    ]
                , exceptionTable = []
                , codeAttributes = []
                }
        ]
