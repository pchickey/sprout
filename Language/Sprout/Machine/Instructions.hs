
module Language.Sprout.Machine.Instructions where

import Data.Word
import Language.Sprout.Expressions

newtype Mem = Mem { unMem :: Word32 }
instance Eq Mem where
  (Mem a) == (Mem b) = a == b
instance Show Mem where
  show (Mem a) = "Mem " ++ (show a)


type Oper = Mem
type Dest = Mem

data Instr
  = IExtern String Dest
  | IConst  Const  Dest
  | IAdd Dest Oper Oper
  | ISub Dest Oper Oper
  | IMul Dest Oper Oper
  | IDiv Dest Oper Oper
  | IAnd Dest Oper Oper
  | INot Dest Oper
  | IEq  Dest Oper Oper
  | ILt  Dest Oper Oper
  | IMux Dest Oper Oper Oper
  deriving (Eq, Show)
