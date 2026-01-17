
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
import StackInstr (stack_All_Instr, stack_chargement, stackInstrLdc, stackInstrIinc)
import ComparInstr
import ConvInstr
import ControlFlowInstr
import IOInstr (invokeWrite)
import qualified Data.Map as Map
import qualified Data.Vector as V


execInstr :: Instr -> VMState -> IO (Either String VMState)

execInstr (ILdc n) st = pure $ stackInstrLdc n st
execInstr (IStck sst) st = pure $ stack_All_Instr sst st
execInstr (IStck_1 sst) st = pure $ stack_chargement sst st

execInstr (INewArray) st = pure $ heapInstrNewArray st
execInstr (IALoad) st = pure $ heapInstrALoad st
execInstr (IAStore) st = pure $ heapInstrAStore st
execInstr (IArrayLength) st = pure $ heapInstrArrayLength st

execInstr (IOpInt op) st = pure $ intArith op st
execInstr (IOpLong op) st = pure $ longArith op st
execInstr (IOpFloat op) st = pure $ floatArith op st
execInstr (IOpDouble op) st = pure $ doubleArith op st

execInstr (IReturn) st = pure $ controlFlowReturn st
execInstr (IReturnInt) st = pure $ controlFlowReturnInt st
execInstr (IReturnLong) st = pure $ controlFlowReturnLong st
execInstr (IReturnFloat) st = pure $ controlFlowReturnFloat st
execInstr (IReturnDouble) st = pure $ controlFlowReturnDouble st
execInstr (IReturnA) st = pure $ controlFlowReturnA st

execInstr (IGoto offset) st = pure $ controlFlowGoto offset st

execInstr (IGetField fieldName) st = pure $ heapInstrGetField fieldName st
execInstr (IPutField fieldName) st = pure $ heapInstrPutField fieldName st
execInstr (INew className) st = pure $ heapInstrNew className st

execInstr (IIfEq n) st = pure $ compIfEq n st
execInstr (IIfNe n) st = pure $ compIfNe n st
execInstr (IIfLt n) st = pure $ compIfLt n st
execInstr (IIfGe n) st = pure $ compIfGe n st
execInstr (IIfGt n) st = pure $ compIfGt n st
execInstr (IIfLe n) st = pure $ compIfLe n st

execInstr (IIfACmpEq n) st = pure $ compIfACmpEq n st
execInstr (IIfACmpNe n) st = pure $ compIfACmpNe n st

execInstr (IIfICmpEq n) st = pure $ compIfICmpEq n st
execInstr (IIfICmpNe n) st = pure $ compIfICmpNe n st
execInstr (IIfICmpLt n) st = pure $ compIfICmpLt n st
execInstr (IIfICmpGe n) st = pure $ compIfICmpGe n st
execInstr (IIfICmpGt n) st = pure $ compIfICmpGt n st
execInstr (IIfICmpLe n) st = pure $ compIfICmpLe n st

execInstr (ILcmp) st = pure $ compLCmp st
execInstr (IFcmpl) st = pure $ compFCmpL st
execInstr (IFcmpg) st = pure $ compFCmpG st

execInstr (IDcmpl) st = pure $ compDCmpL st
execInstr (IDcmpg) st = pure $ compDCmpG st

execInstr (IInvokeStatic funcName) st = pure $ controlFlowInvokeStatic funcName st

execInstr (IConv op) st = pure $ convOp op st

execInstr (IIinc idx inc) st = pure $ stackInstrIinc idx inc st

execInstr (IInvokeWrite fd) st = invokeWrite fd st



exec :: VMState -> IO (Either String VMState)
exec st@VMState{ip, functions, currentFunc} =
    case Map.lookup currentFunc functions of
        Nothing -> pure $ Left ("Function not found: " ++ currentFunc)
        Just func ->
            case (funcCode func) V.!? ip of
                Nothing -> pure $ Left ("Invalid instruction ip: " ++ show ip ++ " in function: " ++ currentFunc)
                Just instr -> execInstr instr st


compile :: VMState -> IO (Either String VMState)
compile vmSt =
  case Map.lookup (currentFunc vmSt) (functions vmSt) of
    Nothing -> pure $ Left ("Function not found: " ++ currentFunc vmSt)
    Just func ->
      if ip vmSt >= V.length (funcCode func)
        then pure $ Right vmSt
        else
          exec vmSt >>= \result ->
          case result of
            Left err  -> pure $ Left err
            Right newvmSt -> compile newvmSt
