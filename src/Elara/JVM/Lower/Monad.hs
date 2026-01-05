{-# LANGUAGE AllowAmbiguousTypes #-}

module Elara.JVM.Lower.Monad where

import Effectful
import Effectful.Error.Static
import Effectful.Writer.Static.Local
import Elara.Data.Unique.Effect
import Elara.JVM.Error (JVMLoweringError)
import Elara.JVM.IR qualified as IR
import Elara.Logging

type Lower r = (UniqueGen :> r, StructuredDebug :> r, Error JVMLoweringError :> r)

type InnerLower r = (Lower r, InstructionWriter :> r)

-- | A Writer effect that collects emitted instructions and extra blocks
type InstructionWriter = Writer ([IR.Instruction], [IR.Block])

emitInst :: InstructionWriter :> r => IR.Instruction -> Eff r ()
emitInst i = tell ([i], [])

-- | Emit a block to the "extra blocks" channel of the Writer
emitBlock :: InstructionWriter :> r => IR.Block -> Eff r ()
emitBlock block = tell ([], [block])
