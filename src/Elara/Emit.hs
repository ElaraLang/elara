-- | Emits JVM bytecode from Elara AST.
module Elara.Emit where

import Control.Lens ((^.))
import Elara.AST.Module (Module)
import Elara.AST.Name (ModuleName (..))
import Elara.AST.Select (HasModuleName (unlocatedModuleName), Typed, moduleName)
import Elara.Data.TopologicalGraph (TopologicalGraph, traverseGraphRevTopologically_)
import JVM.Data.Abstract.ClassFile
import JVM.Data.Abstract.Name (ClassName (ClassName), PackageName (PackageName), QualifiedClassName (QualifiedClassName))
import JVM.Data.JVMVersion
import Polysemy
import Polysemy.Reader
import Polysemy.Writer

type Emit r = Members '[Reader JVMVersion] r

emitGraph :: forall r. Emit r => TopologicalGraph (Module Typed) -> Sem r [ClassFile]
emitGraph g = do
    let tellMod = emitModule >=> tell . one :: Module Typed -> Sem (Writer [ClassFile] : r) () -- this breaks without the type signature lol
    fst <$> runWriter (traverseGraphRevTopologically_ tellMod g)

emitModule :: Emit r => Module Typed -> Sem r ClassFile
emitModule m = do
    let name = createModuleName (m ^. unlocatedModuleName)
    version <- ask
    pure
        ( ClassFile
            name
            version
            []
            Nothing
            []
            []
            []
        )
  where
    createModuleName :: ModuleName -> QualifiedClassName
    createModuleName (ModuleName name) = QualifiedClassName (PackageName $ init name) (ClassName $ last name)
