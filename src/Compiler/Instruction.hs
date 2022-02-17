module Compiler.Instruction where

import Data.Binary.Put
import qualified Data.ByteString.Lazy as L
import Data.Word

data Instruction
  = Return
  | LDC Word8
  | InvokeVirtual Word16

putInstruction :: Instruction -> Put
putInstruction Return = putWord8 0
putInstruction (LDC w) = putWord8 1 >> putWord8 w
putInstruction (InvokeVirtual w) = putWord8 2 >> putWord16be w
