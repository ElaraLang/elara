-- | Emits JVM bytecode from Elara AST.
module Elara.Emit where

import Control.Lens hiding (List)
import Data.List.NonEmpty qualified as NE

import Elara.AST.Name (ModuleName (..))
import Elara.AST.Select (HasModuleName (unlocatedModuleName))
import Elara.AST.VarRef (varRefVal)
import Elara.Data.TopologicalGraph (TopologicalGraph, traverseGraphRevTopologically_)
import Elara.Emit.Operator (translateOperatorName)

import Elara.Core (Bind (..), CoreExpr, Type (..), Var (..))
import Elara.Core.Module (CoreDeclaration (..), CoreModule, declarations)
import Elara.Emit.Var (JVMExpr, transformTopLevelLambdas)
import Elara.Prim.Core (intCon, ioCon, listCon)
import JVM.Data.Abstract.AccessFlags (FieldAccessFlag (FPublic, FStatic), MethodAccessFlag (MPublic, MStatic))
import JVM.Data.Abstract.Builder
import JVM.Data.Abstract.ClassFile
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Field (ClassFileField (ClassFileField))
import JVM.Data.Abstract.Instruction (Instruction (..))
import JVM.Data.Abstract.Method (ClassFileMethod (ClassFileMethod), CodeAttributeData (..), MethodAttribute (..))
import JVM.Data.Abstract.Name (ClassName (ClassName), PackageName (PackageName), QualifiedClassName (QualifiedClassName))
import JVM.Data.Abstract.Type as JVM (ClassInfoType (ClassInfoType), FieldType (ArrayFieldType, ObjectFieldType, PrimitiveFieldType), PrimitiveType (..))
import JVM.Data.JVMVersion
import Polysemy
import Polysemy.Reader
import Polysemy.Writer
import Print (debugPretty, showPretty)

type Emit r = Members '[Reader JVMVersion] r

emitGraph :: forall r. Emit r => TopologicalGraph CoreModule -> Sem r [(ModuleName, ClassFile)]
emitGraph g = do
    let tellMod = emitModule >=> tell . one :: CoreModule -> Sem (Writer [(ModuleName, ClassFile)] : r) () -- this breaks without the type signature lol
    fst <$> runWriter (traverseGraphRevTopologically_ tellMod g)

emitModule :: Emit r => CoreModule -> Sem r (ModuleName, ClassFile)
emitModule m = do
    let name = createModuleName (m ^. unlocatedModuleName)
    version <- ask

    (_, clazz) <- runClassBuilderT name version $ do
        traverse_ addDeclaration (m ^. declarations)
        when (isMainModule m) (addMethod (generateMainMethod m))

    pure
        ( m ^. unlocatedModuleName
        , clazz
        )

generateCodeAttribute :: Emit r => JVMExpr -> ([Instruction] -> [Instruction]) -> ClassBuilderT (Sem r) MethodAttribute
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

addDeclaration :: (Emit r) => CoreDeclaration -> ClassBuilderT (Sem r) ()
addDeclaration declBody = case declBody of
    CoreValue (NonRecursive (Id name type', e)) -> do
        let declName = translateOperatorName $ runIdentity (varRefVal name)
        if typeIsValue type'
            then addField (ClassFileField [FPublic, FStatic] declName (generateFieldType type') [])
            else do
                let descriptor@(MethodDescriptor _ returnType) = generateMethodDescriptor type'
                y <- transformTopLevelLambdas e
                code <- generateCodeAttribute y (if returnType == VoidReturn then (<> [Return]) else (<> [AReturn]))
                addMethod $
                    ClassFileMethod
                        [MPublic, MStatic]
                        declName
                        descriptor
                        [code]
    e -> error (showPretty e)

isMainModule :: CoreModule -> Bool
isMainModule m = m ^. unlocatedModuleName == ModuleName ("Main" :| [])

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

-- createFieldInitialisers :: [Declaration] -> ClassFileMethod
-- createFieldInitialisers decls =
--     ClassFileMethod
--         [MStatic]
--         "<clinit>"
--         (MethodDescriptor [] VoidReturn)
--         [ Code $ CodeAttributeData 255 255 (concatMap generateFieldInitialiser decls ++ [Return]) [] []
--         ]
--   where
--     generateFieldInitialiser :: Declaration -> [Instruction]
--     generateFieldInitialiser d =
--         case d ^. unlocatedDeclarationBody' of
--             Value v
--                 | typeIsValue (v ^. _Expr . _2) ->
--                     let fieldClassName = createModuleName (d ^. unlocatedModuleName)
--                         fieldName = d ^. unlocatedDeclarationName . to nameText
--                         fieldDescriptor = generateFieldType (v ^. _Expr . _2)
--                      in generateCode v <> [PutStatic (ClassInfoType fieldClassName) fieldName fieldDescriptor]
--             _ -> []

createModuleName :: ModuleName -> QualifiedClassName
createModuleName (ModuleName name) = QualifiedClassName (PackageName $ init name) (ClassName $ last name)

-- invokeStaticVars :: CoreExpr -> (ClassInfoType, Text, MethodDescriptor)
-- invokeStaticVars (Expr (Located _ e, exprType)) =
--     case e of
--         AST.Var (Located _ (Global (Located _ (Qualified vn mn)))) ->
--             if typeIsValue exprType
--                 then error "dunno about global vars"
--                 else (ClassInfoType $ createModuleName mn, nameText vn, generateMethodDescriptor exprType)
--         other -> error (showPretty other)

generateCode :: (Emit r) => JVMExpr -> ClassBuilderT (Sem r) [Instruction]
generateCode e = error (showPretty e)

-- generateCode (Expr (Located _ e, exprType)) =
--     case e of
--         FunctionCall
--             (Expr (Located _ (Var (Located _ v)), _))
--             (Expr (Located _ (AST.String "println"), _))
--                 | v == fetchPrimitive ->
--                     [ ALoad0
--                     , InvokeStatic (ClassInfoType "elara.IO") "println" (MethodDescriptor [ObjectFieldType "java.lang.String"] (TypeReturn (ObjectFieldType "elara.IO")))
--                     ]
--         AST.Var (Located _ (Global (Located _ (Qualified n mn)))) -> [GetStatic (ClassInfoType $ createModuleName mn) (nameText n) (generateFieldType exprType)]
--         AST.String c -> [LDC (LDCString c)]
--         AST.Int i -> [LDC (LDCInt (fromIntegral i))]
--         FunctionCall f x -> do
--             let (a, b, c) = invokeStaticVars f
--             let outs = generateCode x
--             outs ++ [InvokeStatic a b c]
--         Lambda _ x -> generateCode x
--         e -> error (showPretty e)

{- | Determines if a type is a value type.
 That is, a type that can be compiled to a field rather than a method.
-}
typeIsValue :: Type -> Bool
typeIsValue c = False

generateFieldType :: Type -> FieldType
generateFieldType c | c == intCon = PrimitiveFieldType JVM.Int
generateFieldType (AppTy l _) | l == listCon = ObjectFieldType "elara.EList"
generateFieldType (TyVarTy _) = ObjectFieldType "java.lang.Object"
generateFieldType (AppTy l _) | l == ioCon = ObjectFieldType "elara.IO"
-- generateFieldType (Scalar _ Bool) = PrimitiveFieldType Boolean
-- generateFieldType (Scalar _ Integer) = PrimitiveFieldType JVM.Int
-- generateFieldType (Scalar _ Natural) = PrimitiveFieldType JVM.Int
-- generateFieldType (Scalar _ Real) = PrimitiveFieldType Double
-- generateFieldType (Scalar _ Monotype.String) = ObjectFieldType "java.lang.String"
-- generateFieldType (Scalar _ Monotype.Char) = PrimitiveFieldType JVM.Char
-- generateFieldType (Custom _ "IO" _) = ObjectFieldType "elara.IO"
-- generateFieldType (VariableType _ _) = ObjectFieldType "java.lang.Object"
-- generateFieldType (Function{}) = ObjectFieldType "elara.Func"
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
