module Register where

import qualified Data.HashMap.Strict as M

type Registers = M.HashMap Char Value

type Value = Integer

data Instruction
  = Mov Char Value
  | MovReg Char Char
  | Inc Char
  | Dec Char
  | Jnz Char Value
  | Err String
  deriving (Eq)

register :: Registers
register = M.fromList (zip ['a' .. 'z'] [0,0 ..])
