{- 
-- EPITECH PROJECT, 2025
-- Stack
-- File description:
-- instr
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module StackInstr (
      stackInstrLoadInt,
      stackInstrStoreInt,
      stackInstrSwap,
      stackInstrPop,
      stackInstrDup,
      stackInstrConstInt,
      stackInstrBipush,
      stackInstrSipush,
      stackInstrLdc,
      stack_All_Instr
) where


import Data
import qualified Data.Vector as V


stackInstrConstInt :: Int -> VMState -> Either String VMState
stackInstrConstInt n st@VMState{stack, ip} =
    Right st { ip = ip + 1, stack = VInt n : stack }


stackInstrBipush :: Int -> VMState -> Either String VMState
stackInstrBipush n st@VMState{stack, ip} =
    Right st { ip = ip + 1, stack = VInt n : stack }


stackInstrSipush :: Int -> VMState -> Either String VMState
stackInstrSipush n st@VMState{stack, ip} =
    Right st { ip = ip + 1, stack = VInt n : stack }


stackInstrLdc :: Int -> VMState -> Either String VMState
stackInstrLdc n st@VMState{stack, ip, constPool} =
    case constPool V.!? n of
        Just val -> Right st { ip = ip + 1, stack = val : stack }
        Nothing  -> Left ("Invalid constant pool index: " ++ show n)


stackInstrLoadInt :: Int -> VMState -> Either String VMState
stackInstrLoadInt n st@VMState{stack, ip, locals} =
    case locals V.!? n of
        Just val -> Right st {ip = (ip + 1), stack = (val : stack)}
        Nothing  -> Left ("Invalid local index: " ++ show n)


stackInstrStoreInt :: Int -> VMState -> Either String VMState
stackInstrStoreInt n st@VMState{stack, ip, locals} =
    case stack of
        [] -> Left "Stack underflow in IStoreInt"
        (first:rest) ->
            case locals V.!? n of
                Nothing -> Left ("Invalid local index: " ++ show n)
                Just _  ->
                    let newLocals = locals V.// [(n, first)]
                    in Right st {ip = (ip + 1), stack = rest, locals = newLocals}


stackInstrSwap :: VMState -> Either String VMState
stackInstrSwap st@VMState{stack, ip} =
    case stack of
        (v1:v2:rest) ->
            Right st {ip = (ip + 1), stack = (v2:v1:rest)}
        _ -> Left "ISwap expects at least two values"


stackInstrPop :: Int -> VMState -> Either String VMState
stackInstrPop n st@VMState{stack, ip} =
    if length stack < n
        then Left ("IPop" ++ show n ++ " expects at least " ++ show n ++ " values")
        else Right st {ip = (ip + 1), stack = (drop n stack)}


stackInstrDup :: WhatDup -> VMState -> Either String VMState
stackInstrDup dup st@VMState{stack, ip} =
    case (dup, stack) of

        (Dup, (v1:rest)) ->
            Right st {ip = (ip + 1), stack = (v1:v1:rest)}

        (Dup2, (v2:v1:rest)) ->
            Right st {ip = (ip + 1), stack = (v2:v1:v2:v1:rest)}

        (DupX1, (v1:v2:rest)) ->
            Right st {ip = (ip + 1), stack = (v1:v2:v1:rest)}

        (DupX2, (v1:v2:v3:rest)) ->
            Right st {ip = (ip + 1), stack = (v1:v3:v2:v1:rest)}

        (Dup2X1, (v1:v2:v3:rest)) ->
            Right st {ip = (ip + 1), stack = (v2:v1:v3:v2:v1:rest)}

        (Dup2X2, (v1:v2:v3:v4:rest)) ->
            Right st {ip = (ip + 1), stack = (v2:v1:v4:v3:v2:v1:rest)}

        _ -> Left "Invalid stack shape for DUP instruction"



stack_All_Instr :: StackIns -> VMState -> Either String VMState
stack_All_Instr ins st =
    case ins of
        IPop      -> stackInstrPop 1 st
        IPop2     -> stackInstrPop 2 st

        IDup      -> stackInstrDup Dup st
        IDupX1    -> stackInstrDup DupX1 st
        IDupX2    -> stackInstrDup DupX2 st
        IDup2     -> stackInstrDup Dup2 st
        IDup2X1   -> stackInstrDup Dup2X1 st
        IDup2X2   -> stackInstrDup Dup2X2 st

        ISwap     -> stackInstrSwap st
        INop      -> Right st
