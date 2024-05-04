{-# LANGUAGE TemplateHaskell #-}

module Elara.Emit.Monad where

import Elara.Data.Pretty
import Elara.Data.Unique
import Elara.Emit.Error
import Elara.Emit.State
import Elara.Emit.Utils (createQualifiedClassName, createQualifiedInnerClassName)
import Elara.Error.Effect (DiagnosticWriter)
import JVM.Data.Abstract.Builder (ClassBuilder, addAttribute, getClass, runClassBuilder)
import JVM.Data.Abstract.ClassFile
import JVM.Data.Abstract.Name (QualifiedClassName)
import JVM.Data.JVMVersion (JVMVersion (JVMVersion))
import Polysemy (Embed, Member, Members, Sem, interpret, makeSem, reinterpret)
import Polysemy.Error
import Polysemy.Log
import Polysemy.Maybe
import Polysemy.Reader (Reader, ask)
import Polysemy.State
import Polysemy.Writer hiding (pass)

type Emit r =
    Members
        '[ Reader JVMVersion
         , Embed IO
         , MaybeE
         , DiagnosticWriter (Doc AnsiStyle)
         , Log
         ]
        r

type InnerEmit r =
    Members
        '[ Reader JVMVersion
         , Reader QualifiedClassName
         , ClassBuilder
         , State CLInitState
         , UniqueGen
         , Error EmitError
         , Log
         , MultiClassBuilder
         ]
        r

newtype CLInitState = CLInitState MethodCreationState

data MultiClassBuilder m a where
    AddBuiltClass :: ClassFile -> MultiClassBuilder m ()

makeSem ''MultiClassBuilder

addClass :: (Member (Reader JVMVersion) r, Member MultiClassBuilder r) => QualifiedClassName -> Sem (ClassBuilder : r) a -> Sem r ()
addClass name sem = do
    ver <- ask @JVMVersion
    (cf, _) <- runClassBuilder name ver sem
    addBuiltClass cf

-- addInnerClass :: (Member MultiClassBuilder r, Member ClassBuilder r) => Sem (ClassBuilder : r) a -> Sem r ()
addInnerClass innerClassName sem = do
    ver <- ask @JVMVersion
    outerClass <- getClass
    let innerClassName' = createQualifiedInnerClassName innerClassName outerClass.name
    (cf, _) <- runClassBuilder innerClassName' ver sem
    addAttribute $ InnerClasses [InnerClassInfo innerClassName' outerClass.name innerClassName mempty]
    addBuiltClass cf
    pass

multiClassBuilderAsWriter :: Sem (MultiClassBuilder : m) r -> Sem (Writer [ClassFile] : m) r
multiClassBuilderAsWriter = reinterpret $ \case
    AddBuiltClass cf -> tell [cf]

runMultiClassBuilder :: Sem (MultiClassBuilder : r) a -> Sem r ([ClassFile], a)
runMultiClassBuilder = runWriter . multiClassBuilderAsWriter
