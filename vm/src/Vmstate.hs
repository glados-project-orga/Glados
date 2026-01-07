
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

execInstr (IPop) st = stackInstrPop 1 st

execInstr (IPop2) st = stackInstrPop 2 st

execInstr (IDup) st =  stackInstrDup Dup st

execInstr (IDupX1) st = stackInstrDup DupX1 st

execInstr (IDupX2) st =  stackInstrDup DupX2 st

execInstr (IDup2X1) st = stackInstrDup Dup2X1 st
    
execInstr (IDup2X2) st = stackInstrDup Dup2X2 st

execInstr (ISwap) st = stackInstrSwap st

execInstr (INop) st = Right st

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
compile vmst =
  if ip vmst >= V.length (code vmst)
    then Right vmst
    else
      case exec vmst of
        Left err  -> Left err
        Right newvmst -> compile newvmst
