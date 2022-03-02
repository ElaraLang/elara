module Compiler.Transform.Environment where

import Compiler.Transform.Abstract
import Compiler.Transform.Types (JVMType)
import Control.Monad.State.Lazy (State)
import qualified Data.Map as M

data CompileState = CompileState
  { classFile :: ClassFile,
    expectedTypes :: M.Map String JVMType
  }

emptyCompileState :: ClassFile -> CompileState
emptyCompileState c =
  CompileState
    { classFile = c,
      expectedTypes = M.empty
    }

type Compiler e r = e -> State CompileState r
