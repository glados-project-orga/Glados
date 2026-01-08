
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
import HeapInstr
import ArithmInt
import StackInstr
import ComparInstr
import ControlFlowInstr
import qualified Data.Vector as V


execInstr :: Instr -> VMState -> Either String VMState

execInstr (IConstInt n) st = stackInstrConstInt n st

execInstr (IBipush n) st = stackInstrBipush n st

execInstr (ISipush n) st = stackInstrSipush n st

execInstr (ILdc n) st = stackInstrLdc n st

execInstr (ILoadInt n) st = stackInstrLoadInt n st

execInstr (IStoreInt n) st = stackInstrStoreInt n st

execInstr (IStck sst) st = stack_All_Instr sst st

execInstr (INewArray) st = heapInstrNewArray st

execInstr (IALoad) st = heapInstrALoad st

execInstr (IAStore) st = heapInstrAStore st

execInstr (IArrayLength) st = heapInstrArrayLength st

execInstr (IOpInt op) st = intArith op st

execInstr IReturn st = controlFlowReturn st

execInstr IReturnInt st = controlFlowReturnInt st

execInstr (IGoto offset) st = controlFlowGoto offset st

execInstr (IGetField fieldName) st = heapInstrGetField fieldName st

execInstr (IPutField fieldName) st = heapInstrPutField fieldName st

execInstr (INew className) st = heapInstrNew className st

execInstr (IIfEq n) st = compIfEq n st
execInstr (IIfNe n) st = compIfNe n st
execInstr (IIfLt n) st = compIfLt n st
execInstr (IIfGe n) st = compIfGe n st
execInstr (IIfGt n) st = compIfGt n st
execInstr (IIfLe n) st = compIfLe n st

execInstr (IIfACmpEq n) st = compIfACmpEq n st
execInstr (IIfACmpNe n) st = compIfACmpNe n st

execInstr (IIfICmpEq n) st = compIfICmpEq n st
execInstr (IIfICmpNe n) st = compIfICmpNe n st
execInstr (IIfICmpLt n) st = compIfICmpLt n st
execInstr (IIfICmpGe n) st = compIfICmpGe n st
execInstr (IIfICmpGt n) st = compIfICmpGt n st
execInstr (IIfICmpLe n) st = compIfICmpLe n st

execInstr (ILcmp) st = compLCmp st
execInstr (IFcmpl) st = compFCmpL st
execInstr (IFcmpg) st = compFCmpG st

execInstr (IDcmpl) st = compDCmpL st
execInstr (IDcmpg) st = compDCmpG st

execInstr _  _ = Left "Invalid instruction or not yet implemented"



exec :: VMState -> Either String VMState
exec st@VMState{ip, code}
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
