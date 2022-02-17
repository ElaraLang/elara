module Compiler.Instruction where

import Data.Binary.Put
import Data.Word

data Instruction
  = Return
  | LDC Word8
  | InvokeVirtual Word16
  | GetStatic Word16

putInstruction :: Instruction -> Put
putInstruction Return = putWord8 177
putInstruction (LDC w) = putWord8 18 >> putWord8 w
putInstruction (InvokeVirtual w) = putWord8 182 >> putWord16be w
putInstruction (GetStatic w) = putWord8 178 >> putWord16be w
