{- | Module for emitting JVM bytecode for lambda expressions.
We differentiate between top level and local lambdas:
Top level lambdas are desugared into static methods on the class representing the module:
otherwise we would end up with a lot of redundant lambda classes
-}
module Elara.JVM.Emit.Lambda where

import Effectful
import Elara.Core (CoreExpr, Var)
import JVM.Data.Abstract.Builder.Code
