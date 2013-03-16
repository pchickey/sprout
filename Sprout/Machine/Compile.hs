
module Sprout.Machine.Compile where


import Sprout.Expressions
import Sprout.Machine.Instructions

import Control.Monad.State.Strict
import Control.Monad.Identity

newtype Program = Program { unProgram :: [Instr] }

type Compiler a = StateT Mem Identity a

memZero :: Mem
memZero = Mem 0
nextMem :: Mem -> Mem
nextMem (Mem n) = Mem (n+1)

compile :: UE -> Program
compile e = fst $ runIdentity $ runStateT f memZero
  where f = obj memZero e

(<>) :: Program -> Program -> Program
(Program p1) <> (Program p2) = Program (p1 ++ p2)


fresh :: Compiler Mem
fresh = do
  m <- get
  put (nextMem m)
  return (nextMem m)

sing :: Instr -> Program
sing i = Program [i]

emit :: Program -> Compiler Program
emit p = return p

emitS :: Instr -> Compiler Program
emitS = emit . sing

-- Create object code
obj :: Dest -> UE -> Compiler Program
obj d e = case e of
  UConst c           -> emitS $ IConst  c    d
  UVRef  (UV name _) -> emitS $ IExtern name d
  UAdd   e1  e2      -> binop d IAdd e1 e2
  USub   e1  e2      -> binop d ISub e1 e2
  UMul   e1  e2      -> binop d IMul e1 e2
  UDiv   e1  e2      -> binop d IDiv e1 e2
  UAnd   e1  e2      -> binop d IAnd e1 e2
  UNot   e1          -> unaryop d INot e1
  UEq    e1  e2      -> binop d IEq  e1 e2
  ULt    e1  e2      -> binop d ILt  e1 e2
  UMux   e1  e2  e3  -> muxop d e1 e2 e3

binop :: Dest -> (Dest -> Oper -> Oper -> Instr) -> UE -> UE -> Compiler Program
binop d c e1 e2 = do
  d1 <- fresh
  d2 <- fresh
  p1 <- obj d1 e1
  p2 <- obj d2 e2
  emit (p1 <> p2 <> sing (c d d1 d2))

unaryop :: Dest -> (Dest -> Oper -> Instr) -> UE -> Compiler Program
unaryop d c e = do
  d' <- fresh
  p' <- obj d' e
  emit (p' <> sing (c d d'))

muxop :: Dest -> UE -> UE -> UE -> Compiler Program
muxop d e1 e2 e3 = do
  d1 <- fresh
  d2 <- fresh
  d3 <- fresh
  p1 <- obj d1 e1
  p2 <- obj d2 e2
  p3 <- obj d3 e3
  emit $ p1 <> p2 <> p3 <> sing (IMux d d1 d2 d3)
