-- | Emits JVM bytecode from Elara AST.
-- Conventions:
-- * We always uncurry functions, so a function of type a -> b -> c is compiled to a JVM function c f(a, b).
-- This means that currying can be avoided in a lot of cases (through arity analysis),
--   skipping the overhead of creating a lambda and calling repeated invoke() functions
module Elara.Emit where

import Control.Lens hiding (List)
import Data.Generics.Product
import Elara.AST.Name (ModuleName (..))
import Elara.AST.VarRef (varRefVal)
import Elara.Core (Bind (..), Expr (..), Type (..), Var (..))
import Elara.Core.Module (CoreDeclaration (..), CoreModule)
import Elara.Core.Pretty ()
import Elara.Data.TopologicalGraph (TopologicalGraph, traverseGraphRevTopologically_)
import Elara.Emit.Expr
import Elara.Emit.Operator (translateOperatorName)
import Elara.Emit.Utils
import Elara.Emit.Var (JVMBinder (..), JVMExpr, transformTopLevelLambdas)
import Elara.Prim.Core (intCon, ioCon, stringCon)
import JVM.Data.Abstract.Builder
import JVM.Data.Abstract.ClassFile
import JVM.Data.Abstract.ClassFile.AccessFlags
import JVM.Data.Abstract.ClassFile.Field
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Instruction (Instruction (..))
import JVM.Data.Abstract.Name (QualifiedClassName)
import JVM.Data.Abstract.Type as JVM (ClassInfoType (ClassInfoType), FieldType (ArrayFieldType, ObjectFieldType))
import JVM.Data.JVMVersion
import Polysemy
import Polysemy.Reader
import Polysemy.Writer (Writer, runWriter, tell)
import Print (showPretty)

type Emit r = Members '[Reader JVMVersion] r

type InnerEmit a =
  Sem
    '[ Writer [Instruction], -- <clinit> instructions
       Reader JVMVersion,
       Reader QualifiedClassName,
       Embed ClassBuilder
     ]
    a

emitGraph :: forall r. (Emit r) => TopologicalGraph CoreModule -> Sem r [(ModuleName, ClassFile)]
emitGraph g = do
  let tellMod = emitModule >=> tell . one :: CoreModule -> Sem (Writer [(ModuleName, ClassFile)] : r) () -- this breaks without the type signature lol
  fst <$> runWriter (traverseGraphRevTopologically_ tellMod g)

emitModule :: (Emit r) => CoreModule -> Sem r (ModuleName, ClassFile)
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
    ( m ^. field @"name",
      clazz
    )

addClinit :: (Member (Embed ClassBuilder) r) => [Instruction] -> Sem r ()
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
  code <- codeMod <$> embed (generateInstructions e)
  pure $
    Code $
      CodeAttributeData
        { maxStack = 10, -- TODO: calculate this
          maxLocals = 3, -- TODO: calculate this too
          code = code,
          exceptionTable = [],
          codeAttributes = []
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
  code <- embed $ generateInstructions e
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
          { maxStack = 10, -- TODO: calculate this
            maxLocals = 10, -- TODO: calculate this too
            code =
              [ GetStatic (ClassInfoType (createModuleName (m ^. field @"name"))) "main" (ObjectFieldType "elara.IO"),
                InvokeVirtual (ClassInfoType "elara.IO") "run" (MethodDescriptor [] VoidReturn),
                Return
              ],
            exceptionTable = [],
            codeAttributes = []
          }
    ]

-- | Determines if a type is a value type.
-- That is, a type that can be compiled to a field rather than a method.
typeIsValue :: Type -> Bool
typeIsValue (AppTy con _) | con == ioCon = True
typeIsValue c | c == stringCon = True
typeIsValue c | c == intCon = True
typeIsValue _ = False
