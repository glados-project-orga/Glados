
{- 
-- EPITECH PROJECT, 2025
-- vm
-- File description:
-- State
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Vmstate (
    compile
) where

import Data
import Data.Bits
import HeapInstr
import ArithmInt
import StackInstr
import qualified Data.Vector as V


execInstr :: Instr -> VMState -> Either String VMState

execInstr (IConstInt n) st@VMState{stack, ip} =
    Right st {ip = (ip + 1), stack = (VInt n : stack)}

execInstr (ILoadInt n) st = stackInstrLoadInt n st

execInstr (IStoreInt n) st = stackInstrStoreInt n st

execInstr (IStck sst) st = stack_All_Instr sst st

execInstr (INewArray) st = heapInstrNewArray st

execInstr (IALoad) st = heapInstrALoad st

execInstr (IAStore) st = heapInstrAStore st

execInstr (IArrayLength) st = heapInstrArrayLength st

execInstr (IOpInt op) st = intArith op st


execInstr _  _ = Left "Invalid instruction or not yet implemented"
exec :: VMState -> Either String VMState
exec st@VMState{stack, locals, ip, code, heap, frames}
    | Just instr <- code V.!? ip = execInstr instr st
    | otherwise = Left ("Invalid instruction ip: " ++ show ip)


compile :: VMState -> Either String VMState
compile vmSt =
  if ip vmSt >= V.length (code vmSt)
    then Right vmSt
    else
      case exec vmSt of
        Left err  -> Left err
        Right newvmSt -> compile newvmSt
