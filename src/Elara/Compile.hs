module Elara.Compile where

import Control.Lens (to, traverseOf, traverseOf_, (^.))
import Data.Binary.Put (runPut)
import Data.Binary.Write (WriteBinary (writeBinary))
import Elara.AST.Module (Module, traverseModule_)
import Elara.AST.Name (HasName (name), ModuleName (ModuleName), NameLike (nameText), Qualified (_qualifiedName))
import Elara.AST.Region (Located (Located), unlocated)
import Elara.AST.Select (HasModuleName (moduleName), Typed)
import Elara.AST.Typed qualified as Typed
import JVM.Data.Abstract.AccessFlags (FieldAccessFlag (..))
import JVM.Data.Abstract.ClassFile (ClassFile (ClassFile), fields)
import JVM.Data.Abstract.Field (ClassFileField (..), ConstantValue (..), FieldAttribute (..))
import JVM.Data.Abstract.Name (ClassName (ClassName), PackageName (PackageName), QualifiedClassName (..), suitableFilePath)
import JVM.Data.Abstract.Type (BaseType (..), FieldType (..))
import JVM.Data.Convert (convert)
import JVM.Data.JVMVersion (java8)
import Polysemy (Member, Sem, embed)
import Polysemy.Embed (Embed)
import Polysemy.State

moduleNameToJVMName :: ModuleName -> QualifiedClassName
moduleNameToJVMName (ModuleName (n :| [])) = QualifiedClassName (PackageName []) (ClassName n)
moduleNameToJVMName (ModuleName ns) = QualifiedClassName (PackageName (init ns)) (ClassName (last ns))

compileModule :: (Member (Embed IO) r) => Module Typed -> Sem r ()
compileModule m = do
    let jvmName = moduleNameToJVMName (m ^. moduleName . unlocated)
    let classFile = ClassFile jvmName java8 [] Nothing [] [] []
    classFile' <- execState classFile (traverseModule_ addDeclarationToClass m)
    let converted = convert classFile'
    let bs = runPut (writeBinary converted)
    let fp = suitableFilePath jvmName
    embed (writeFileLBS fp bs)

addDeclarationToClass :: forall r. (Member (State ClassFile) r) => Typed.Declaration -> Sem r ()
addDeclarationToClass (Typed.Declaration ld) =
    traverseOf_ unlocated addDeclarationToClass' ld
  where
    addDeclarationToClass' :: Typed.Declaration' -> Sem r ()
    addDeclarationToClass' (Typed.Declaration' _ n (Typed.DeclarationBody (Located _ (Typed.Value (Typed.Expr (e, ty)))))) = do
        let field =
                ClassFileField
                    [FPublic, FStatic, FFinal]
                    (n ^. unlocated . to _qualifiedName . to nameText)
                    (jvmTypeOf (e ^. unlocated))
                    (ConstantValue <$> maybeToList (constantValueOf (e ^. unlocated)))

        modify (\s -> s{fields = fields s <> [field]})
    addDeclarationToClass' _ = pass

jvmTypeOf :: Typed.Expr' -> FieldType
jvmTypeOf (Typed.Int _) = BaseFieldType Int
jvmTypeOf (Typed.String _) = ObjectFieldType "java/lang/String"
jvmTypeOf (Typed.Char _) = BaseFieldType Char
jvmTypeOf (Typed.Float _) = BaseFieldType Double
jvmTypeOf _ = error "aaaa"

constantValueOf :: Typed.Expr' -> Maybe ConstantValue
constantValueOf (Typed.Int i) = Just (ConstantInteger (fromIntegral i))
constantValueOf (Typed.String s) = Just (ConstantString s)
constantValueOf (Typed.Char c) = Just (ConstantInteger (fromIntegral (fromEnum c)))
constantValueOf (Typed.Float f) = Just (ConstantDouble f)
constantValueOf _ = Nothing
