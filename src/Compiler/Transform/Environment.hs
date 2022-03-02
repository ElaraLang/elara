module Compiler.Transform.Environment where

import Compiler.Transform.Abstract

type Compiler e = e -> ClassFile -> ClassFile
