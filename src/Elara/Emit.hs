-- | Emits JVM bytecode from Elara AST.
module Elara.Emit where

import Control.Lens hiding (List)
import Data.List.NonEmpty qualified as NE

import Elara.AST.Name (ModuleName (..), NameLike (nameText), Qualified (..))
import Elara.AST.Select (HasModuleName (unlocatedModuleName))
import Elara.AST.VarRef (VarRef' (..), varRefVal)
import Elara.Data.TopologicalGraph (TopologicalGraph, traverseGraphRevTopologically_)
import Elara.Emit.Operator (translateOperatorName)

import Elara.Core (Bind (..), Expr (..), Literal (..), Type (..), Var (..))
import Elara.Core.Module (CoreDeclaration (..), CoreModule, declarations)
import Elara.Emit.Var (JVMBinder (..), JVMExpr, transformTopLevelLambdas)
import Elara.Prim.Core (intCon, ioCon, listCon, stringCon)
import Elara.Utils (uncurry3)
import JVM.Data.Abstract.AccessFlags (FieldAccessFlag (FPublic, FStatic), MethodAccessFlag (MPublic, MStatic))
import JVM.Data.Abstract.Builder
import JVM.Data.Abstract.ClassFile
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Field (ClassFileField (ClassFileField))
import JVM.Data.Abstract.Instruction (Instruction (..), LDCEntry (LDCString))
import JVM.Data.Abstract.Method (ClassFileMethod (ClassFileMethod), CodeAttributeData (..), MethodAttribute (..))
import JVM.Data.Abstract.Name (ClassName (ClassName), PackageName (PackageName), QualifiedClassName (QualifiedClassName))
import JVM.Data.Abstract.Type as JVM (ClassInfoType (ClassInfoType), FieldType (ArrayFieldType, ObjectFieldType, PrimitiveFieldType), PrimitiveType (..))
import JVM.Data.JVMVersion
import Polysemy
import Polysemy.Reader
import Polysemy.Writer
import Print (debugColored, debugPretty, showPretty)

type Emit r = Members '[Reader JVMVersion] r

type InnerEmit a =
    Sem
        '[ Writer [Instruction] -- <clinit> instructions
         , Reader JVMVersion
         , Reader QualifiedClassName
         , Embed ClassBuilder
         ]
        a

emitGraph :: forall r. Emit r => TopologicalGraph CoreModule -> Sem r [(ModuleName, ClassFile)]
emitGraph g = do
    let tellMod = emitModule >=> tell . one :: CoreModule -> Sem (Writer [(ModuleName, ClassFile)] : r) () -- this breaks without the type signature lol
    fst <$> runWriter (traverseGraphRevTopologically_ tellMod g)

emitModule :: Emit r => CoreModule -> Sem r (ModuleName, ClassFile)
emitModule m = do
    let name = createModuleName (m ^. unlocatedModuleName)
    version <- ask

    let runInnerEmit =
            runClassBuilder name version
                . runM
                . runReader name
                . runReader version

    let (_, clazz) = runInnerEmit $ do
            (clinit, _) <- runWriter @[Instruction] $ do
                traverse_ addDeclaration (m ^. declarations)
                when (isMainModule m) (embed $ addMethod (generateMainMethod m))

            addClinit clinit

    pure
        ( m ^. unlocatedModuleName
        , clazz
        )

addClinit :: Member (Embed ClassBuilder) r => [Instruction] -> Sem r ()
addClinit code = do
    embed $
        addMethod $
            ClassFileMethod
                [MPublic, MStatic]
                "<clinit>"
                (MethodDescriptor [] VoidReturn)
                [Code $ CodeAttributeData 255 255 (code <> [Return]) [] []]

generateCodeAttribute :: JVMExpr -> ([Instruction] -> [Instruction]) -> InnerEmit MethodAttribute
generateCodeAttribute e codeMod = do
    code <- codeMod <$> generateCode e
    pure $
        Code $
            CodeAttributeData
                { maxStack = 2 -- TODO: calculate this
                , maxLocals = 2 -- TODO: calculate this too
                , code = code
                , exceptionTable = []
                , codeAttributes = []
                }

addDeclaration :: CoreDeclaration -> InnerEmit ()
addDeclaration declBody = case declBody of
    CoreValue (NonRecursive (Id name type', e)) -> do
        let declName = translateOperatorName $ runIdentity (varRefVal name)
        if typeIsValue type'
            then do
                let field = ClassFileField [FPublic, FStatic] declName (generateFieldType type') []
                embed $ addField field
                e' <- transformTopLevelLambdas e
                addStaticFieldInitialiser field e'
            else do
                let descriptor@(MethodDescriptor _ returnType) = generateMethodDescriptor type'
                y <- transformTopLevelLambdas e
                code <- generateCodeAttribute y (if returnType == VoidReturn then (<> [Return]) else (<> [AReturn]))
                embed $
                    addMethod $
                        ClassFileMethod
                            [MPublic, MStatic]
                            declName
                            descriptor
                            [code]
    e -> error (showPretty e)

isMainModule :: CoreModule -> Bool
isMainModule m = m ^. unlocatedModuleName == ModuleName ("Main" :| [])

-- | Adds an initialiser for a static field to <clinit>
addStaticFieldInitialiser :: ClassFileField -> JVMExpr -> InnerEmit ()
addStaticFieldInitialiser (ClassFileField _ name fieldType _) e = do
    code <- generateCode e
    tell code
    cn <- ask @QualifiedClassName
    tell [PutStatic (ClassInfoType cn) name fieldType]

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
                { maxStack = 2 -- TODO: calculate this
                , maxLocals = 2 -- TODO: calculate this too
                , code =
                    [ GetStatic (ClassInfoType (createModuleName (m ^. unlocatedModuleName))) "main" (ObjectFieldType "elara.IO")
                    , InvokeVirtual (ClassInfoType "elara.IO") "run" (MethodDescriptor [] VoidReturn)
                    , Return
                    ]
                , exceptionTable = []
                , codeAttributes = []
                }
        ]

createModuleName :: ModuleName -> QualifiedClassName
createModuleName (ModuleName name) = QualifiedClassName (PackageName $ init name) (ClassName $ last name)

generateCode :: JVMExpr -> InnerEmit [Instruction]
generateCode (Var (JVMLocal 0)) = pure [ALoad0]
generateCode e@(App (Var (Normal (Id{idVarName = Global (Identity (Qualified vn mn)), idVarType = idVarType}))) x) = do
    -- static function application
    let invokeStaticVars = (ClassInfoType $ createModuleName mn, vn, generateMethodDescriptor idVarType)
    x' <- generateCode x

    pure $ x' <> [uncurry3 InvokeStatic invokeStaticVars, CheckCast (ClassInfoType "java.lang.String")]
generateCode (Lit (String s)) =
    pure
        [ LDC (LDCString s)
        ]
generateCode e = error (showPretty e)

{- | Determines if a type is a value type.
 That is, a type that can be compiled to a field rather than a method.
-}
typeIsValue :: Type -> Bool
typeIsValue c | c == stringCon = True
typeIsValue _ = False

generateFieldType :: Type -> FieldType
generateFieldType c | c == intCon = PrimitiveFieldType JVM.Int
generateFieldType c | c == stringCon = ObjectFieldType "java.lang.String"
generateFieldType (AppTy l _) | l == listCon = ObjectFieldType "elara.EList"
generateFieldType (TyVarTy _) = ObjectFieldType "java.lang.Object"
generateFieldType (AppTy l _) | l == ioCon = ObjectFieldType "elara.IO"
generateFieldType o = error $ "generateFieldType: " <> show o

generateMethodDescriptor :: HasCallStack => Type -> MethodDescriptor
generateMethodDescriptor (ForAllTy _ t) = generateMethodDescriptor t
generateMethodDescriptor f@(FuncTy _ _) = do
    -- (a -> b) -> [a] -> [b] gets compiled to List<B> f(Func<A, B> f, List<A> l)
    let
        splitUpFunction :: Type -> NonEmpty Type
        splitUpFunction (FuncTy i o) = i `NE.cons` splitUpFunction o
        splitUpFunction other = pure other

        allParts = splitUpFunction f

        inputs = init allParts
        output = last allParts

    MethodDescriptor (generateFieldType <$> inputs) (generateReturnDescriptor output)
generateMethodDescriptor o = error $ "generateMethodDescriptor: " <> show o

generateReturnDescriptor :: Type -> ReturnDescriptor
-- generateReturnDescriptor (Scalar _ Unit) = VoidReturn
generateReturnDescriptor other = TypeReturn (generateFieldType other)
