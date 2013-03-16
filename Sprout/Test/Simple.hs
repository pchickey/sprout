
module Sprout.Test.Simple where

import Data.Int
import Data.Word

import qualified Data.Vector as V

import Sprout.Expressions
import Sprout.Machine.Instructions
import Sprout.Machine.Compile
import Sprout.Machine.Simulate

test1 :: E Int32
test1 = Add (ConstE 1) (ConstE 2)

test2 :: E Word32
test2 = Add (ConstE 2) (ConstE 1)

test3 :: E Bool
test3 = And (ConstE True) (ConstE True)

test4 :: E Bool
test4 = And (ConstE True) (Not (Not (ConstE True)))

test5 :: E Bool
test5 = And (ConstE False) (Not (Not (ConstE True)))

test6 :: E Bool
test6 = And (ConstE True) (Not (Not (ConstE False)))

test7 :: E Int32
test7 = Sub (ConstE 1) (ConstE 2)

test8 :: E Word32
test8 = Sub (ConstE 1) (ConstE 2)

doTest :: (Expr a) => E a -> IO()
doTest expr = do
  let (u, i, s) = runTest expr
  print u
  print i
  print (take 20 (V.toList s))

runTest :: (Expr a) => E a -> (UE, [Instr], MachineState)
runTest expr = (toue, toins, finalstate)
  where
  toue = ue expr
  toins = unProgram $ compile toue
  finalstate = run toins emptyMachineState
