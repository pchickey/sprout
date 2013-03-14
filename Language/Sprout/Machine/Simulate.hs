
module Language.Sprout.Machine.Simulate 
  ( MachineState
  , emptyMachineState
  , step
  , run
  ) where

import Prelude hiding (read)

import Data.Word
import qualified Data.Vector as V
import Data.Vector (Vector, (//), (!))
import Language.Sprout.Machine.Instructions
import Language.Sprout.Expressions (Const(..))

type MachineState = Vector Word32

emptyMachineState :: MachineState
emptyMachineState = V.fromListN 1024 (repeat 0)

run :: [Instr] -> MachineState -> MachineState
run program initialstate = foldl step initialstate program

step :: MachineState -> Instr -> MachineState
step s i = case i of
  IExtern _ d           -> write d 666 -- XXX need an environment
  IConst  (CBool b) d   -> write d $ if b then 1 else 0
  IConst  (CInt32 l) d  -> write d $ fromIntegral l
  IConst  (CWord32 l) d -> write d l
  
  IAdd d a b  -> write d $ (read a) + (read b)
  ISub d a b  -> write d $ (read a) - (read b)
  IMul d a b  -> write d $ (read a) * (read b)
  IDiv d a b  -> write d $ (read a) `div` (read b)

  IAnd d a b  -> write d $ iand (read a) (read b)
  INot d a    -> write d $ inot (read a)

  IEq  d a b  -> write d $ if (read a) == (read b) then 1 else 0
  ILt  d a b  -> write d $ if (read a) <  (read b) then 1 else 0

  IMux d a b c -> write d $ imux (read a) (read b) (read c)

  where
  write :: Dest -> Word32 -> MachineState
  write d v = s // [(fromIntegral (unMem d), v)]

  read ::Oper -> Word32
  read o = s ! (fromIntegral (unMem o))

  iand :: Word32 -> Word32 -> Word32
  iand 0 _ = 0
  iand _ 0 = 0
  iand _ _ = 1

  inot :: Word32 -> Word32
  inot 0 = 1
  inot _ = 0

  imux :: Word32 -> Word32 -> Word32 -> Word32
  imux 0 a _ = a
  imux _ _ b = b

