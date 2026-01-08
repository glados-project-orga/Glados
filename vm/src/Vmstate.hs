
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
import ControlFlowInstr
import qualified Data.Map as Map
import qualified Data.Vector as V


execInstr :: Instr -> VMState -> Either String VMState

execInstr (IConstInt n) st = stackInstrConstInt n st

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

execInstr (IInvokeStatic funcName) st = controlFlowInvokeStatic funcName st

execInstr _  _ = Left "Invalid instruction or not yet implemented"

exec :: VMState -> Either String VMState
exec st@VMState{ip, functions, currentFunc} =
    case Map.lookup currentFunc functions of
        Nothing -> Left ("Function not found: " ++ currentFunc)
        Just func ->
            case (funcCode func) V.!? ip of
                Nothing -> Left ("Invalid instruction ip: " ++ show ip ++ " in function: " ++ currentFunc)
                Just instr -> execInstr instr st


compile :: VMState -> Either String VMState
compile vmSt =
  case Map.lookup (currentFunc vmSt) (functions vmSt) of
    Nothing -> Left ("Function not found: " ++ currentFunc vmSt)
    Just func ->
      if ip vmSt >= V.length (funcCode func)
        then Right vmSt
        else
          case exec vmSt of
            Left err  -> Left err
            Right newvmSt -> compile newvmSt
