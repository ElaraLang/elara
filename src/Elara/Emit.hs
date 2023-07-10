-- | Emits JVM bytecode from Elara AST.
module Elara.Emit where

import Control.Lens (to, (^.))
import Elara.AST.Lenses (HasDeclarationBody' (unlocatedDeclarationBody'))
import Elara.AST.Module (HasDeclarations (declarations), Module)
import Elara.AST.Name (ModuleName (..), Name (..), NameLike (nameText), Qualified (Qualified), VarName (NormalVarName))
import Elara.AST.Region (Located (Located), unlocated)
import Elara.AST.Select (HasDeclarationName (unlocatedDeclarationName), HasModuleName (unlocatedModuleName), Typed)
import Elara.AST.Typed as AST (Declaration, DeclarationBody' (..), Expr (..), Expr' (FunctionCall, String, Var), _Expr)
import Elara.AST.VarRef (VarRef' (Global))
import Elara.Data.TopologicalGraph (TopologicalGraph, traverseGraphRevTopologically_)
import Elara.Prim (fetchPrimitive)
import Elara.TypeInfer.Monotype as Monotype (Scalar (..))
import Elara.TypeInfer.Type (Type (..))
import JVM.Data.Abstract.AccessFlags (FieldAccessFlag (FPublic, FStatic), MethodAccessFlag (MPublic, MStatic))
import JVM.Data.Abstract.ClassFile
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Field (ClassFileField (ClassFileField))
import JVM.Data.Abstract.Instruction (Instruction (..), LDCEntry (LDCString))
import JVM.Data.Abstract.Method (ClassFileMethod (ClassFileMethod), CodeAttributeData (..), MethodAttribute (..))
import JVM.Data.Abstract.Name (ClassName (ClassName), PackageName (PackageName), QualifiedClassName (QualifiedClassName))
import JVM.Data.Abstract.Type as JVM (ClassInfoType (ClassInfoType), FieldType (ArrayFieldType, ObjectFieldType, PrimitiveFieldType), PrimitiveType (..))
import JVM.Data.JVMVersion
import JVM.Data.Raw.ConstantPool (ConstantPoolInfo (ClassInfo))
import Polysemy
import Polysemy.Reader
import Polysemy.Writer
import Print (debugColored, showPretty)

type Emit r = Members '[Reader JVMVersion] r

emitGraph :: forall r. Emit r => TopologicalGraph (Module Typed) -> Sem r [(ModuleName, ClassFile)]
emitGraph g = do
  let tellMod = emitModule >=> tell . one :: Module Typed -> Sem (Writer [(ModuleName, ClassFile)] : r) () -- this breaks without the type signature lol
  fst <$> runWriter (traverseGraphRevTopologically_ tellMod g)

emitModule :: Emit r => Module Typed -> Sem r (ModuleName, ClassFile)
emitModule m = do
  let name = createModuleName (m ^. unlocatedModuleName)
  version <- ask

  let (fields, methods) = partitionEithers $ createMethodOrField <$> m ^. declarations
  pure
    ( m ^. unlocatedModuleName,
      ClassFile
        name
        version
        []
        Nothing
        []
        fields
        methods
        []
    )
  where
    createMethodOrField :: Declaration -> Either ClassFileField ClassFileMethod
    createMethodOrField d =
      case d ^. unlocatedDeclarationBody' of
        TypeDeclaration {} -> Left undefined
        Value v ->
          let (expr, type') = v ^. _Expr
           in if d ^. unlocatedDeclarationName == Qualified (NVarName (NormalVarName "main")) (ModuleName ("Main" :| []))
                then -- special case, generate main method
                case type' of
                  Custom _ "IO" _ ->
                    Right $
                      ClassFileMethod
                        [MPublic, MStatic]
                        "main"
                        ( MethodDescriptor
                            [ArrayFieldType (ObjectFieldType "java.lang.String")]
                            VoidReturn
                        )
                        [generateCodeAttribute v (<> [InvokeVirtual (ClassInfoType "elara.IO") "run" (MethodDescriptor [] VoidReturn), Return])]
                  _ -> error "main must have type IO a"
                else
                  if typeIsValue type'
                    then Left $ ClassFileField [FPublic, FStatic] (d ^. unlocatedDeclarationName . to nameText) (toFieldType type') []
                    else
                      let descriptor@(MethodDescriptor _ returnType) = generateMethodDescriptor type'
                       in Right $
                            ClassFileMethod
                              [MPublic, MStatic]
                              (d ^. unlocatedDeclarationName . to nameText)
                              descriptor
                              [generateCodeAttribute v (if returnType == VoidReturn then (<> [Return]) else (<> [AReturn]))]

    generateCodeAttribute :: Expr -> ([Instruction] -> [Instruction]) -> MethodAttribute
    generateCodeAttribute e codeMod =
      Code $
        CodeAttributeData
          { maxStack = 2, -- TODO: calculate this
            maxLocals = 2, -- TODO: calculate this too
            code = codeMod (generateCode e),
            exceptionTable = [],
            codeAttributes = []
          }

generateMainMethod :: ClassFileMethod
generateMainMethod =
  ClassFileMethod
    [MPublic, MStatic]
    "main"
    ( MethodDescriptor
        [ArrayFieldType (ObjectFieldType "java.lang.String")]
        VoidReturn
    )
    [ Code $
        CodeAttributeData
          { maxStack = 2, -- TODO: calculate this
            maxLocals = 2, -- TODO: calculate this too
            code =
              [ ALoad0,
                InvokeStatic (ClassInfoType "elara.IO") "println" (MethodDescriptor [ObjectFieldType "java.lang.String"] (TypeReturn (ObjectFieldType "elara.IO")))
              ],
            exceptionTable = [],
            codeAttributes = []
          }
    ]

createModuleName :: ModuleName -> QualifiedClassName
createModuleName (ModuleName name) = QualifiedClassName (PackageName $ init name) (ClassName $ last name)

invokeStaticVars :: Expr -> (ClassInfoType, Text, MethodDescriptor)
invokeStaticVars (Expr (Located _ e, exprType)) =
  case e of
    AST.Var (Located _ (Global (Located _ (Qualified vn mn)))) ->
      if typeIsValue exprType
        then error "dunno about global vars"
        else (ClassInfoType $ createModuleName mn, nameText vn, generateMethodDescriptor exprType)
    other -> error (show other)

generateCode :: Expr -> [Instruction]
generateCode (Expr (Located _ e, exprType)) =
  case e of
    FunctionCall
      (Expr (Located _ (Var (Located _ v)), _))
      (Expr (Located _ (AST.String "println"), _))
        | v == fetchPrimitive ->
            [ ALoad0,
              InvokeStatic (ClassInfoType "elara.IO") "println" (MethodDescriptor [ObjectFieldType "java.lang.String"] (TypeReturn (ObjectFieldType "elara.IO")))
            ]
    AST.Var (Located _ (Global qn)) -> error "standalone var"
    AST.String c -> [LDC (LDCString c)]
    FunctionCall f x -> do
      let (a, b, c) = invokeStaticVars f
      let outs = generateCode x
      outs ++ [InvokeStatic a b c]
    e -> error (show e)

-- | Determines if a type is a value type.
-- That is, a type that can be compiled to a field rather than a method.
typeIsValue :: Type a -> Bool
typeIsValue (Scalar _ _) = True
typeIsValue (Record _ _) = True
typeIsValue (Tuple _ _) = True
typeIsValue ((Custom _ "IO" _)) = True
typeIsValue _ = False

generateMethodDescriptor :: Show a => Type a -> MethodDescriptor
generateMethodDescriptor (Function _ i o) = MethodDescriptor (toFieldType <$> collapseFunctions i) (generateReturnDescriptor o)
  where
    collapseFunctions :: Type a -> [Type a]
    collapseFunctions (Function _ i o) = i : collapseFunctions o
    collapseFunctions other = [other]
generateMethodDescriptor o = error $ "generateMethodDescriptor: " <> show o

generateReturnDescriptor :: Show a => Type a -> ReturnDescriptor
generateReturnDescriptor (Scalar _ Unit) = VoidReturn
generateReturnDescriptor other = TypeReturn (toFieldType other)

toFieldType :: Show a => Type a -> FieldType
toFieldType (Scalar _ Bool) = PrimitiveFieldType Boolean
toFieldType (Scalar _ Integer) = PrimitiveFieldType Int
toFieldType (Scalar _ Natural) = PrimitiveFieldType Int
toFieldType (Scalar _ Real) = PrimitiveFieldType Double
toFieldType (Scalar _ Monotype.String) = ObjectFieldType "java.lang.String"
toFieldType (Scalar _ Monotype.Char) = PrimitiveFieldType JVM.Char
toFieldType (Custom _ "IO" _) = ObjectFieldType "elara.IO"
toFieldType other = error $ "don't know how to convert toFieldType: " <> show other
