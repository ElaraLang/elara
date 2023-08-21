-- | Emits JVM bytecode from Elara AST.
module Elara.Emit where

import Control.Lens hiding (List)
import Data.List.NonEmpty qualified as NE

import Elara.AST.Name (ModuleName (..), Qualified (..))
import Elara.AST.VarRef (VarRef' (..), varRefVal)
import Elara.Data.TopologicalGraph (TopologicalGraph, traverseGraphRevTopologically_)
import Elara.Emit.Operator (translateOperatorName)

import Data.Generics.Product
import Elara.Core (Bind (..), Expr (..), Literal (..), Type (..), Var (..))
import Elara.Core.Module (CoreDeclaration (..), CoreModule, declarations)
import Elara.Emit.Var (JVMBinder (..), JVMExpr, transformTopLevelLambdas)
import Elara.Prim.Core (fetchPrimitiveName, intCon, ioCon, listCon, stringCon)
import Elara.Utils (uncurry3)
import JVM.Data.Abstract.AccessFlags (FieldAccessFlag (FPublic, FStatic), MethodAccessFlag (MPublic, MStatic))
import JVM.Data.Abstract.Builder
import JVM.Data.Abstract.ClassFile
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Field (ClassFileField (ClassFileField))
import JVM.Data.Abstract.Instruction (Instruction (..), LDCEntry (LDCInt, LDCString))
import JVM.Data.Abstract.Method (ClassFileMethod (ClassFileMethod), CodeAttributeData (..), MethodAttribute (..))
import JVM.Data.Abstract.Name (ClassName (ClassName), PackageName (PackageName), QualifiedClassName (QualifiedClassName))
import JVM.Data.Abstract.Type as JVM (ClassInfoType (ClassInfoType), FieldType (ArrayFieldType, ObjectFieldType))
import JVM.Data.JVMVersion
import Polysemy
import Polysemy.Reader
import Polysemy.Writer
import Print (showPretty)

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
    let name = createModuleName (m ^. field' @"name")
    version <- ask

    let runInnerEmit =
            runClassBuilder name version
                . runM
                . runReader name
                . runReader version

    let (_, clazz) = runInnerEmit $ do
            (clinit, _) <- runWriter @[Instruction] $ do
                traverse_ addDeclaration (m ^. field @"declarations")
                when (isMainModule m) (embed $ addMethod (generateMainMethod m))

            addClinit clinit

    pure
        ( m ^. field @"name"
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

generateCodeAttribute :: JVMExpr -> Maybe Type -> ([Instruction] -> [Instruction]) -> InnerEmit MethodAttribute
generateCodeAttribute e expected codeMod = do
    code <- codeMod <$> generateCode e expected
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
                addStaticFieldInitialiser field e' type'
            else do
                let descriptor@(MethodDescriptor _ returnType) = generateMethodDescriptor type'
                y <- transformTopLevelLambdas e
                code <- generateCodeAttribute y (Just type') (if returnType == VoidReturn then (<> [Return]) else (<> [AReturn]))
                embed $
                    addMethod $
                        ClassFileMethod
                            [MPublic, MStatic]
                            declName
                            descriptor
                            [code]
    e -> error (showPretty e)

isMainModule :: CoreModule -> Bool
isMainModule m = m ^. field @"name" == ModuleName ("Main" :| [])

-- | Adds an initialiser for a static field to <clinit>
addStaticFieldInitialiser :: ClassFileField -> JVMExpr -> Type -> InnerEmit ()
addStaticFieldInitialiser (ClassFileField _ name fieldType _) e t = do
    code <- generateCode e (Just t)
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
                    [ GetStatic (ClassInfoType (createModuleName (m ^. field @"name"))) "main" (ObjectFieldType "elara.IO")
                    , InvokeVirtual (ClassInfoType "elara.IO") "run" (MethodDescriptor [] VoidReturn)
                    , Return
                    ]
                , exceptionTable = []
                , codeAttributes = []
                }
        ]

createModuleName :: ModuleName -> QualifiedClassName
createModuleName (ModuleName name) = QualifiedClassName (PackageName $ init name) (ClassName $ last name)

generateCode :: JVMExpr -> Maybe Type -> InnerEmit [Instruction]
generateCode (Var (JVMLocal 0)) _ = pure [ALoad0]
-- Hardcode elaraPrimitive "println"
generateCode (App (Var (Normal (Id (Global (Identity v)) _))) (Lit (String "println"))) _
    | v == fetchPrimitiveName =
        pure
            [ ALoad0
            , InvokeStatic (ClassInfoType "elara.IO") "println" (MethodDescriptor [ObjectFieldType "java.lang.String"] (TypeReturn (ObjectFieldType "elara.IO")))
            ]
generateCode ((Var (Normal (Id{idVarName = Global (Identity (Qualified vn mn)), idVarType = idVarType})))) _ = do
    -- load static var
    let invokeStaticVars = (ClassInfoType $ createModuleName mn, vn, generateFieldType idVarType)
    pure [uncurry3 GetStatic invokeStaticVars]
generateCode (App (Var (Normal (Id{idVarName = Global (Identity (Qualified vn mn)), idVarType = idVarType}))) arg) (Just expectedType) = do
    -- static function application
    let invokeStaticVars = (ClassInfoType $ createModuleName mn, vn, generateMethodDescriptor idVarType)

    x' <- generateCode arg Nothing

    let castInstrs =
            ( case generateFieldType expectedType of
                ObjectFieldType a -> [CheckCast (ClassInfoType a)]
                _ -> []
            )

    pure $ x' <> [uncurry3 InvokeStatic invokeStaticVars] <> castInstrs
generateCode (Lit (String s)) _ =
    pure
        [ LDC (LDCString s)
        ]
generateCode (Lit (Int i)) _ =
    pure
        [ LDC (LDCInt (fromIntegral i))
        , InvokeStatic (ClassInfoType "java.lang.Integer") "valueOf" (MethodDescriptor [ObjectFieldType "java.lang.String"] (TypeReturn (ObjectFieldType "java.lang.Integer")))
        ]
generateCode e t = error (showPretty (e, t))

{- | Determines if a type is a value type.
 That is, a type that can be compiled to a field rather than a method.
-}
typeIsValue :: Type -> Bool
typeIsValue (AppTy con _) | con == ioCon = True
typeIsValue c | c == stringCon = True
typeIsValue c | c == intCon = True
typeIsValue _ = False

generateFieldType :: Type -> FieldType
generateFieldType c | c == intCon = ObjectFieldType "java.lang.Integer"
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
